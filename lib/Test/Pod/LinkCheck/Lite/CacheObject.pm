package Test::Pod::LinkCheck::Lite::CacheObject;

use 5.008;

use strict;
use warnings;

use Carp ();

our $VERSION = '0.014_003';

sub __new {
    my ( $class, $arg ) = @_;
    return bless $arg, $class;
}

foreach my $attr ( qw{
    created_at
    expires_at
    key
    value
    } ) {
    eval <<"EOD"	## no critic (ProhibitStringyEval)
sub $attr { return \$_[0]{'$attr'}; }
sub get_$attr { return \$_[0]{'$attr'}; }
1;
EOD
	or Carp::confess( "Bug - $@" );
}

sub is_expired {
    my ( $self ) = @_;
    return time >= $self->expires_at();
}

1;

__END__

=head1 NAME

Test::Pod::LinkCheck::Lite::CacheObject - Cache entry

=head1 SYNOPSIS

No user-serviceable parts inside.

=head1 DESCRIPTION

This Perl class is B<private> to the C<Test-Pod-LinkCheck-Lite> package,
and may be changed or revoked without notice. Documentation is for the
convenience of the author, and does not constitute a commitment to the
user. I<Caveat coder.>

This Perl class represents an entry in a cache. The interface
deliberately reproduces the L<CHI::CacheObject|CHI::CacheObject>
interface as of that module's version C<0.61>.

=head1 METHODS

This class supports the following package-private methods:

=head2 __new

This static method instantiates the object. Arguments are specified as a
hash reference. The hash B<must> contain values for B<only> the
following keys.

=over

=item key

This is the key of the cache entry, B<before> any munging by the cache
driver.

=item value

This is the value stored under that key.

=item created_at

This is the epoch time at which the cache entry was created.

=item expires_at

This is the epoch time at which the cache entry expires.

=back

=head2 key

This method returns the key of the cache entry.

=head2 value

This method returns the value stored in the cache entry.

=head2 created_at

This method returns the epoch time at which the cache entry was created.

=head2 expires_at

This method returns the epoch time at which the cache entry expires.

=head2 get_key

This method is a synonym for L<key()|/key>.

=head2 get_value

This method is a synonym for L<value()|/value>.

=head2 get_created_at

This method is a synonym for L<created_at()|/created_at>.

=head2 get_expires_at

This method is a synonym for L<expires_at()|/expires_at>.

=head2 is_expired

This method returns a true value if the entry is expired, and a false
value otherwise.

=head1 SEE ALSO

L<Test::Pod::LinkCheck::Lite::Cache::File|Test::Pod::LinkCheck::Lite::Cache::File>

L<CHI::CacheObject|CHI::CacheObject>

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
