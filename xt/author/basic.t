package main;

use 5.008;

use strict;
use warnings;

use Test::More 0.88;	# Because of done_testing();

diag 'Modules required for author testing';

# require_ok 'CPANPLUS';

require_ok 'HTTP::Tiny';

require_ok 'LWP::UserAgent';

done_testing;

1;

# ex: set textwidth=72 :
