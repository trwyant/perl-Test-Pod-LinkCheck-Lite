package Test::Pod::LinkCheck::Lite::Cache::File;

use 5.008;

use strict;
use warnings;

use parent qw{ Test::Pod::LinkCheck::Lite::Cache };	# Core since 5.10.1

use Carp ();			# Core since 5.0
use Digest::MD5 ();		# Core since 5.7.3
use Errno qw{ ENOENT };	# Core since 5.5
use File::Path ();		# Core since 5.7.3.
use File::Spec;			# Core since 5.4.5
use File::stat;			# Core since 5.4
use Storable ();		# Core since 5.7.3
use Test::Pod::LinkCheck::Lite::CacheObject;

our $VERSION = '0.014_004';

{
    my %attribute = (
	root_dir	=> {
	    default	=> sub {
		my ( $self ) = @_;
		( my $caller = $self->{_caller} ) =~ s/::/_/smxg;
		return File::Spec->catdir(
		    File::Spec->tmpdir(),
		    $caller,
		);
	    },
	    validate	=> sub {
		my ( $self, undef, $value ) = @_;
		$self->{_cache_dir} = File::Spec->catdir(
		    $value, $self->{namespace},
		);
		return;
	    },
	},
    );

    sub __init {
	my ( $self, $arg ) = @_;
	$self->SUPER::__init( $arg );
	$self->SUPER::__init( $arg, \%attribute );
	return;
    }
}

sub clear {
    my ( $self ) = @_;
    # FIXME CHI::Driver::File renames the name space directory to a temp
    # and then deletes it. I have the problem that the current
    # implementation actually deletes the name space directory and then
    # recreates it, which is a potential race. I have to do it this way
    # because things like set() assume the name space directory exists,
    # whereas CHI's set() creates it if necessary.

    # CHI::Driver::File renames the directory first and then deletes it,
    # as being 'more atomic'.
    my $temp_dir = File::Spec->catdir(
	$self->{root_dir},
	"_temp_$self->{namespace}",
    );
    if ( rename $self->{_cache_dir}, $temp_dir ) {
	File::Path::rmtree( $temp_dir );
    } elsif ( $! == ENOENT ) {
	# No name space found -- we're good.
    } else {
	Carp::croak(
	    "Failed to rename $self->{_cache_dir} to $temp_dir: $!" );
    }
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

sub __get {
    my ( $self, $key ) = @_;
    my $path = $self->_path_to_key( $key );
    if (
	my $stat = stat $path
	    and
	open my $fh, '<', $path	## no critic (RequireBriefOpen,RequireCheckedOpen)
    ) {
	my $hash = Storable::fd_retrieve( $fh );
	$hash->{created_at} = $stat->mtime();
	$hash->{expires_at} = $hash->{created_at} + $self->{expires_in};
	return Test::Pod::LinkCheck::Lite::CacheObject->__new( $hash );
    }
    $! == ENOENT
	and return undef;	## no critic (ProhibitExplicitReturnUndef)
    Carp::croak( "Failed to open $path: $!" );
}

sub get_keys {
    my ( $self ) = @_;
    my @keys;
    if ( opendir my $dh, $self->{_cache_dir} ) {
	while ( defined( my $key = readdir $dh ) ) {
	    $key =~ m/ [^[:xdigit:]] /smx
		and next;
	    push @keys, $key;
	}
	closedir $dh;
	return @keys;
    } elsif ( $! == ENOENT ) {
	return @keys;
    } else {
	Carp::croak( "Failed to open $self->{_cache_dir}: $!" );
    }
}

sub remove {
    my ( $self, $key ) = @_;
    my $path = $self->_path_to_key( $key );
    unlink $path
	and return $self;
    $! == ENOENT
	and return $self;
    my $err = $!;
    my $obj = $self->__get( $key )
	or Carp::croak( "Failed to retrieve $path: $!" );
    Carp::croak( "Failed to remove @{[ $obj->key() ]}: $err" );
}

sub __set {
    my ( $self, $hash ) = @_;
    my $path = $self->_path_to_key( $hash->{key} );
    # CHI::Driver::File writes to a temp file and then renames it,
    # as being a "more atomic" way to do things. CHI::Driver::File uses
    # Perl's rename() rather than Perl::Copy::move(), and I think I can
    # here too. CHI::Driver::File uses non-core module Data::UUID to
    # generate the name of the temp file, but given my file naming
    # convention, suffixing '-temp' should work.
    my $temp_path = "$path-temp";
    Storable::store_fd(
	$hash,
	$self->_open_for_output( $temp_path ),
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

sub _make_cache_dir {
    my ( $self ) = @_;
    -d $self->{_cache_dir}
	and return;
    # The documentation is unclear on how mkpath() handles an error. My
    # read of the code says that it throws an exception given the way we
    # are calling it.
    File::Path::mkpath( $self->{_cache_dir} );
    # But I'm being paranoid.
    my $err = $!;
    -d $self->{_cache_dir}
	and return;
    Carp::croak( "Unable to create $self->{_cache_dir}: $err" );
}

sub _open_for_output {
    my ( $self, $path ) = @_;
    my $fh;
    open $fh, '>', $path	## no critic (RequireBriefOpen,RequireCheckedOpen)
	and return $fh;
    if ( $! == ENOENT ) {
	$self->_make_cache_dir();
	open $fh, '>', $path	## no critic (RequireBriefOpen,RequireCheckedOpen)
	    and return $fh;
    }
    Carp::croak( "Unable to open $path: $!" );
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

=head2 new

 my $cache = Test::Pod::LinkCheck::Lite::Cache::File->new();

This static method instantiates the cache object. It adds the following
arguments (as name/value pairs) to those of the superclass:

=over

=item root_dir

This argument specifies the root of the cache. The default is computed
by appending the caller's package name (with C<::> munged to C<_>) to
C<File::Spec->tmpdir()>. B<Note> that this default is incompatible with
L<CHI|CHI>.

=back

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
