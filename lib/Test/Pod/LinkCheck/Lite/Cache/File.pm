package Test::Pod::LinkCheck::Lite::Cache::File;

use 5.008;

use strict;
use warnings;

use Carp ();			# Core since 5.0
use Digest::MD5 ();		# Core since 5.7.3
use Errno qw{ ENOENT };	# Core since 5.5
use File::Path ();		# Core since 5.7.3.
use File::Spec;			# Core since 5.4.5
use File::stat;			# Core since 5.4
use Storable ();		# Core since 5.7.3
use Test::Pod::LinkCheck::Lite::CacheObject;

our $VERSION = '0.014_003';

sub new {
    my ( $class, %arg ) = @_;
    defined $arg{expires_in}
	or $arg{expires_in} = 86_400;	# One day
    defined $arg{namespace}
	and $arg{namespace} ne ''
	or $arg{namespace} = 'Default';
    unless ( defined $arg{root_dir} ) {
	( my $caller = caller ) =~ s/::/_/smxg;
	$arg{root_dir} = File::Spec->catdir(
	    File::Spec->tmpdir(),
	    $caller,
	);
    }
    my $self = { map { $_ => delete $arg{$_} } qw{ expires_in namespace
	root_dir } };
    local $" = ', ';
    my @extra;
    @extra = sort keys %arg
	and Carp::croak( "Unsupported arguments @extra" );
    my $path = File::Spec->catdir(
	map { $self->{$_} } qw{ root_dir namespace } );
    $self->{_cache_dir} = $path;
    -d $path
	or File::Path::mkpath( $path )
	or Carp::croak( "Unable to create $path: $!" );
    return bless $self, $class;
}

sub clear {
    my ( $self ) = @_;
    # FIXME CHI::Driver::File renames the name space directory to a temp
    # and then deletes it. I have the problem that the current
    # implementation actually deletes the name space directory and then
    # recreates it, which is a potential race. I have to do it this way
    # because things like set() assume the name space directory exists,
    # whereas CHI's set() creates it if necessary.
    # FIXME actually there is another problem with this implementatoin:
    # a potential race with another process that wants to add something
    # to the namespace in the gap between the time it was deleted and
    # the time it is recreated.
    # FIXME potential fix: Storable::nstore throws an exception if the
    # store fails, leaving the error code in $!. The error is ENOENT if
    # the directory is missing. Trap the exception, test for ENOENT, and
    # create the directory.
    File::Path::rmtree( $self->{_cache_dir} );
    # The -d test is pure paranoia on my part. rmtree() is documented as
    # removing the root of the tree.
    -d $self->{_cache_dir}
	or File::Path::mkpath( $self->{_cache_dir} );
    return $self;
}

sub expire {
    my ( $self, $key ) = @_;
    my $path = $self->_path_to_key( $key );
    $self->_is_valid( $path )
	or return $self;
    my $utime = time - $self->{expires_in};
    utime $utime, $utime, $path
	and return $self;
    Carp::croak( "Failed to expire $key: $!" );
}

sub get {
    my ( $self, $key, %arg ) = @_;
    my $path = $self->_path_to_key( $key );
    my $obj;
    if ( my $stat = stat $path ) {
	my $hash = Storable::retrieve( $path );
	$hash->{created_at} = $stat->mtime();
	$hash->{expires_at} = $hash->{created_at} + $self->{expires_in};
	$obj = Test::Pod::LinkCheck::Lite::CacheObject->__new( $hash );
    }
    $arg{obj_ref}
	and ${$arg{obj_ref}} = $obj;
    $obj
	and not $obj->is_expired()
	and return $obj->value();
    return undef;	## no critic (ProhibitExplicitReturnUndef)
}

sub get_keys {
    my ( $self ) = @_;
    opendir my $dh, $self->{_cache_dir}
	or Carp::croak( "Failed to open $self->{_cache_dir}: $!" );
    my @keys;
    while ( defined( my $key = readdir $dh ) ) {
	$key =~ m/ [^[:xdigit:]] /smx
	    and next;
	push @keys, $key;
    }
    closedir $dh;
    return @keys;
}

sub purge {
    my ( $self ) = @_;
    foreach my $key_hash ( $self->get_keys() ) {
	my $path = $self->_path_to_key( $key_hash );
	$self->_is_valid( $path )
	    and next;
	unlink $path
	    and next;
	$! == ENOENT
	    and next;
	my $hash = Storable::retrieve( $path );
	Carp::carp( "Failed to purge $hash->{key}: $!" );
    }
    return $self;
}

sub remove {
    my ( $self, $key ) = @_;
    my $path = $self->_path_to_key( $key );
    unless ( unlink $path or $! == ENOENT ) {
	Carp::croak( "Failed to remove $key: $!" );
    }
    return $self;
}

sub set {
    my ( $self, $key, $value ) = @_;
    my $path = $self->_path_to_key( $key );
    # CHI::Driver::File writes to a temp file and then renames it,
    # as being a "more atomic" way to do things. CHI::Driver::File uses
    # Perl's rename() rather than Perl::Copy::move(), and I think I can
    # here too. CHI::Driver::File uses non-core module Data::UUID to
    # generate the name of the temp file, but given my file naming
    # convention, suffixing '-temp' should work.
    my $temp_path = "$path-temp";
    Storable::nstore( {
	    key  => $key,
	    value => $value,
	},
	$temp_path,
    );

    # CHI::Driver::File retries the rename, claiming a rare race
    # condition under unspecified circumstances, so I will too.
    for ( 1 .. 3 ) {
	rename $temp_path, $path
	    and return $self;
    }

    my $err = $!;
    unlink $temp_path;
    Carp::croak( "Unable to rename $temp_path to $path: $err" );
}

sub _is_valid {
    my ( $self, $path ) = @_;

    my $stat = stat $path
	or return;
    $stat->mtime() + $self->{expires_in} > time
	and return 1;
    return;
}

sub _path_to_key {
    my ( $self, $key ) = @_;
    # This seems dodgy to me, but CHI::Driver::File does it.
    # Actually, I guess it's OK for this application because the keys
    # are absolute URLs, which must contain a colon. I think in general
    # though I would have get_keys() return an object that stringifies
    # to the hash, and use the object-versus-scalar distinction to
    # decided whether to hashify the key.
    length( $key ) == 32
	and $key !~ m/ [^[:xdigit:]] /smx
	or $key = Digest::MD5::md5_hex( $key );
    return File::Spec->catfile( $self->{_cache_dir}, $key );
}

1;

__END__

=head1 NAME

Test::Pod::LinkCheck::Lite::Cache::File - File-based cache for Test::Pod::LinkCheck::Lite

=head1 SYNOPSIS

No user-serviceable parts inside.

=head1 DESCRIPTION

This Perl class is B<private> to the C<Test-Pod-LinkCheck-Lite> package,
and may be changed or revoked without notice. Documentation is for the
convenience of the author, and does not constitute a commitment to the
user. I<Caveat coder.>

This Perl class implements an on-disk cache using subset of the
L<CHI|CHI> interface, but without the latter's dependencies. All known
dependencies are core given a version of Perl that is not positively
ancient.

=head1 METHODS

This class supports the following package-private methods:

=head2 new

 my $cache = Test::Pod::LinkCheck::Lite::Cache::File->new();

This static method instantiates the cache object. It takes the following
arguments as name/value pairs:

=over

=item expires_in

This argument is the number of seconds after which the cache entry is
expired. The default is one day. B<Note> that this implementation does
not support per-entry expiration times.

=item namespace

This argument specifies a name space in which this object's entries
reside, Unlike L<CHI|CHI>, this must be a valid file name element. The
default is C<'Default'>, oddly enough.

=item root_dir

This argument specifies the root of the cache. The default is computed
by appending the caller's package name (with C<::> munged to C<_>) to
C<File::Spec->tmpdir()>. B<Note> that this default is incompatible with
L<CHI|CHI>.

=back

An exception will be thrown if any other arguments are specified.

=head2 clear

 $cache->clear();

This method clears the name space.

=head2 expire

 $cache->expire( $key );

This method expires the given key. It is not an error to expire a
non-existent key.

=head2 get

 my $value = $cache->get( $key );

This method returns the value associated with the given key. If there is
none, or if it is expired, nothing is returned.

=head2 get_keys

 print "$_\n" for sort $cache->get_keys();

This method returns all keys in the cache, including the keys of expired
items. B<Note> that these will probably not be the original keys, for
the cache entries but can still be used to retrieve them.

=head2 purge

 $cache->purge();

This method removes all expired entries from the cache.

=head2 remove

 $cache->remove( $key );

This method removes the given key from the cache. It is not an error if
the key is not in the cache.

=head2 set

 $cache->set( $key, $value );

This method adds the given value to the cache under the given key. If
the key is already present in the cache, its value is replaced.

=head1 SEE ALSO

L<CHI|CHI>

L<Cache::FileCache|Cache::FileCache>

=head1 SUPPORT

Support is by the author. Please file bug reports at
L<https://github.com/trwyant/perl-Test-Pod-LinkCheck-Lite/issues> or in
electronic mail to the author.

=head1 AUTHOR

Thomas R. Wyant, III F<wyant at cpan dot org>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2026 by Thomas R. Wyant, III

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl 5.10.0. For more details, see the full text
of the licenses in the files F<LICENSE-Artistic> and F<LICENSE-GPL>.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.

=cut

# ex: set textwidth=72 :
