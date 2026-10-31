package Test::Pod::LinkCheck::Lite::Cache;

use 5.008;

use strict;
use warnings;

use Carp ();			# Core since 5.0

our $VERSION = '0.014_004';

sub new {
    my ( $class, %arg ) = @_;
    my $self = bless {}, $class;
    $self->{_caller} = caller;
    $self->__init( \%arg );
    my @extra = sort keys %arg
	or return $self;
    local $" = ', ';
    Carp::croak( "Unsupported arguments @extra" );
}

{
    my %attribute = (
	expires_in	=> {
	    default	=> sub { 86_400 },	# One day
	},
	namespace	=> {
	    default	=> sub { 'Default' },
	    validate	=> sub {
		my ( undef, $name, $value ) = @_;	# Invocant unused
		$value =~ m/ \A [[:alpha:]0-9] /smx
		    and return;
		Carp::croak( "'$name' must begin with an alphanumeric" );
	    },
	},
    );

    sub __init {
	my ( $self, $arg, $attr ) = @_;
	$attr ||= \%attribute;
	foreach my $name ( keys %$attr ) {
	    my $value = delete $arg->{$name};
	    if ( defined( $value ) && $value ne '' ) {
		$attr->{$name}{validate}
		    and $attr->{$name}{validate}->( $self, $name, $value );
	    } else {
		$attr->{$name}{default}
		    and $value = $attr->{$name}{default}->( $self,
		    $name, $value );
	    }
	    $self->{$name} = $value;
	}
	return;
    }
}

sub clear {
    Carp::confess( 'Bug - this method must be overridden' );
}

sub expire {
    Carp::confess( 'Bug - this method must be overridden' );
}

sub get {
    my ( $self, $key, %arg ) = @_;
    my $obj = $self->__get( $key );
    $arg{obj_ref}
	and ${$arg{obj_ref}} = $obj;
    $obj
	and not $obj->is_expired()
	and return $obj->value();
    return undef;	## no critic (ProhibitExplicitReturnUndef)
}

# $self->__get( $key )
# returns a Test::Pod::LinkCheck::Lite::CacheObject if one exists for
# that key, or undef if it does not. Throws an exception for any other
# error.
sub __get {
    Carp::confess( 'Bug - this method must be overridden' );
}

sub get_keys {
    Carp::confess( 'Bug - this method must be overridden' );
}

sub purge {
    my ( $self ) = @_;
    foreach my $key ( $self->get_keys() ) {
	my $obj = $self->__get( $key )
	    or next;
	$obj->is_expired()
	    or next;
	$self->remove( $obj->key() );
    }
    return $self;
}

sub remove {
    Carp::confess( 'Bug - this method must be overridden' );
}

{
    my %special = (
	now	=> sub { 0 },
	never	=> sub { 0 + 'Inf' },
    );
    sub set {
	my ( $self, $key, $value, $expires_in ) = @_;
	defined $expires_in
	    or $expires_in = $self->{expires_in};
	$special{$expires_in}
	    and $expires_in = $special{$expires_in}->();
	my $created_at = time;
	$self->__set( {
		key	=> $key,
		value	=> $value,
		created_at	=> $created_at,
		expires_at	=> $created_at + $expires_in,
	    } );
	return $self;
    }
}

# $self->__set( $hash ), where the keys are {key}, {value},
# {created_at}, and {expires_at}.
sub __set {
    Carp::confess( 'Bug - this method must be overridden' );
}

1;

__END__

=head1 NAME

Test::Pod::LinkCheck::Lite::Cache - Abstract cache for Test::Pod::LinkCheck::Lite

=head1 SYNOPSIS

No user-serviceable parts inside.

=head1 DESCRIPTION

This Perl class is B<private> to the C<Test-Pod-LinkCheck-Lite> package,
and may be changed or revoked without notice. Documentation is for the
convenience of the author, and does not constitute a commitment to the
user. I<Caveat coder.>

This Perl class defines the API for caching for the
C<Test-Pod-LinkCheck-Lite> package. It should not be instantiated on its
own.

The intent is to provide a subset of the C<CHI|CHI> API to the extent
feasible.

=head1 METHODS

This class provides the following package-private methods:

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
reside, Unlike L<CHI|CHI>, this must be a valid file name element, and
must begin with an alphanumeric. The default is C<'Default'>, oddly
enough.

=back

B<Note> that subclasses may define their own arguments in addition to
these. An exception will be thrown if any unknown arguments are
specified.

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

<<< replace or remove boilerplate >>>

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
