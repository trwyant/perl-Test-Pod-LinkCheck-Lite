package main;

use 5.008;

use strict;
use warnings;

use Test::Pod::LinkCheck::Lite;
use Test::More 0.88;	# Because of done_testing();

use constant STRICT_IS_POSSIBLE	=>
    Test::Pod::LinkCheck::Lite->__strict_is_possible();

my @ua = ( undef );
{
    local $@ = undef;
    eval {
	require HTTP::Tiny;
	push @ua, 'HTTP::Tiny';
    };
    eval {
	require LWP::UserAgent;
	push @ua, 'LWP::UserAgent';
    };
}

{
    my $t = Test::Pod::LinkCheck::Lite->new(
	strict	=> STRICT_IS_POSSIBLE,
    );

    $t->pod_file_ok( \'' );

    $t->pod_file_ok( 't/data/empty.pod' );

    $t->pod_file_ok( 't/data/no_links.pod' );

    $t->pod_file_ok( 't/data/url_links.pod' );

    SKIP: {
	$t->man()
	    or skip 'This system does not support the testing of man links', 1;

	$t->pod_file_ok( 't/data/man.pod' );
    }

    $t->pod_file_ok( 't/data/internal.pod' );

    # This circumlocution will be used for tests where errors are
    # expected.  Unfortunately it only tests that the correct number of
    # errors are reported, not that the errors reported are the correct
    # ones.

    {
	my $errors;

	TODO: {
	    local $TODO = 'Deliberate test failures.';
	    $errors = $t->pod_file_ok( 't/data/internal_error.pod' );
	}

	cmp_ok $errors, '==', 2, 't/data/internal_error.pod had 2 errors';
    }

    $t->pod_file_ok( 't/data/external_builtin.pod' );

    $t->pod_file_ok( 't/data/external_installed.pod' );

    $t->pod_file_ok( 't/data/external_installed_section.pod' );

    {
	my $errors;

	TODO: {
	    local $TODO = 'Deliberate test failures.';
	    $errors = $t->pod_file_ok(
		't/data/external_installed_bad_section.pod' );
	}

	cmp_ok $errors, '==', 1,
	    't/data/external_installed_bad_section.pod had 1 error';
    }

    $t->pod_file_ok( 't/data/external_installed_pod.pod' );

    $t->pod_file_ok( 't/data/external_uninstalled.pod' );

    $t->all_pod_files_ok();

}

foreach my $ua ( @ua ) {
    my $t = Test::Pod::LinkCheck::Lite->new(
	strict	=> STRICT_IS_POSSIBLE && $ua,
	ua	=> $ua,
    );

    note 'Test with explicitly-specified ua => ',
	defined $ua ? "'$ua'" : 'undef';

    $t->pod_file_ok( 't/data/url_links.pod' );
}

foreach my $mi ( Test::Pod::LinkCheck::Lite->new()->module_index() ) {
    my $t = Test::Pod::LinkCheck::Lite->new(
	module_index	=> $mi,
	strict		=> $ENV{AUTHOR_TESTING},
    );

    note "Test with module_index => $mi";

    $t->pod_file_ok( 't/data/external_uninstalled.pod' );
}

done_testing;

1;

# ex: set textwidth=72 :
