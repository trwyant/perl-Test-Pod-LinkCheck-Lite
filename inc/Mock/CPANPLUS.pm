package CPANPLUS;

use 5.008;

use strict;
use warnings;

use Carp;

our $VERSION = '0.000_01';


1;

__END__

=head1 NAME

CPANPLUS - Mock CPANPLUS class

=head1 SYNOPSIS

 use lib qw{ inc/Mock };

 use CPANPLUS;

 ...

=head1 DESCRIPTION

This Perl class mocks whatever portion of the L<CPANPLUS|CPANPLUS>
interface is needed by
L<Test::Pod::LinkCheck::Lite|Test::Pod::LinkCheck::Lite>. It is private
to that distribution, and may change or be revoked without notice.
Documentation is for the benefit of the author.

No actual functionality lives here, since
L<Test::Pod::LinkCheck::Lite|Test::Pod::LinkCheck::Lite> does not use
any C<CPANPLUS> functionality. But
L<Test::Pod::LinkCheck::Lite|Test::Pod::LinkCheck::Lite> will not use a
C<CPANPLUS> index unless it can load the module. So we give it a module
to load.

=head1 METHODS

This class supports no public methods whatsoever.

=head1 SEE ALSO

L<CPANPLUS|CPANPLUS> (the real one).

=head1 SUPPORT

Support is by the author. Please file bug reports at
L<http://rt.cpan.org>, or in electronic mail to the author.

=head1 AUTHOR

Thomas R. Wyant, III F<wyant at cpan dot org>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2019 by Thomas R. Wyant, III

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl 5.10.0. For more details, see the full text
of the licenses in the directory LICENSES.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.

=cut

# ex: set textwidth=72 :
