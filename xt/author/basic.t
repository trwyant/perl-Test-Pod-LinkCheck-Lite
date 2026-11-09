package main;

use 5.008;

use strict;
use warnings;

use Test::More 0.88;	# Because of done_testing();

use lib qw{ inc };
use My::Module::Meta;

diag 'Modules required for author testing';

note 'Optional modules.';

require_ok $_ for My::Module::Meta->optional_modules();

note 'Author testing modules. This list needs filling out.';

require_ok $_ for qw{
    Test::Prereq::Meta
};

done_testing;

1;

# ex: set textwidth=72 :
