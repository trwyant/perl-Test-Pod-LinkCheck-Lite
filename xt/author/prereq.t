package main;

use strict;
use warnings;

use Test::More 0.88;	# Because of done_testing();
use Test::Prereq::Meta;

use lib qw{ inc };
use My::Module::Meta;

my $tpm = Test::Prereq::Meta->new(
    accept	=> [ My::Module::Meta->optional_modules() ],
);

$tpm->all_prereq_ok();

$tpm->all_prereqs_used();

done_testing;

1;

# ex: set textwidth=72 :
