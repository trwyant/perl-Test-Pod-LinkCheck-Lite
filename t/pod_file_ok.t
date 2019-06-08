package main;

use 5.008;

use strict;
use warnings;

use Test::More 0.88;	# Because of done_testing();

# This mess is because if Devel::Hide or Test::Without::Module is
# specified on the command line or in an enclosing file, a straight
# 'use lib qw{ inc/Mock }' would trump it, and the mocked modules would
# still be loaded. With this mess, the command-line version is
# $ perl -Mlib=inc/Mock -MDevel::Hide=HTTP::Tiny,LWP::UserAgent ...,
# and the 'use if' sees inc/Mock already in @INC and does not add it
# again.  'use if' is core as of 5.6.2, so I should be OK unless I run
# into some Linux packager who knows better than the Perl Porters what
# should be in core (and yes, they exist).

use constant CODE_REF	=> ref sub {};

{
    my $inx = 0;
    OUTER_LOOP: {
	while ( $inx < @INC ) {
	    CODE_REF eq ref $INC[$inx++]
		and last OUTER_LOOP;
	}
	$inx = 0;
    }
    splice @INC, $inx, 0, 'inc/Mock';
}


{
    no warnings qw{ once };

    local $Test::Pod::LinkCheck::Lite::DIRECTORY_LEADER = '_';
    require Test::Pod::LinkCheck::Lite;

    require Storable;	# Core, so should always have
    eval {
	require CPANPLUS;	# Mocked

	$Storable::VERSION = 42;	# For ease of testing CPANPLUS
	$CPANPLUS::VERSION = 42;	# For ease of testing CPANPLUS
    };
}

my $STRICT_IS_POSSIBLE	=
    Test::Pod::LinkCheck::Lite->__strict_is_possible();


note '$STRICT_IS_POSSIBLE is ', $STRICT_IS_POSSIBLE ? 'true' : 'false';

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
    local $ENV{HOME} = 't/data';
    local $ENV{PERL5_CPANPLUS_HOME} = 't/data';

    my $t = Test::Pod::LinkCheck::Lite->new(
	strict	=> $STRICT_IS_POSSIBLE,
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
	strict	=> $STRICT_IS_POSSIBLE,
	ua	=> $ua,
    );

    note 'Test with explicitly-specified ua => ',
	defined $ua ? "'$ua'" : 'undef';

    if ( $ua ) {
	$t->pod_file_ok( 't/data/url_links.pod' );
    } else {
	my $errors;

	TODO: {
	    local $TODO = $STRICT_IS_POSSIBLE ?
		'Deliberate test failures.' : undef;
	    $errors = $t->pod_file_ok(
		't/data/url_links.pod' );
	}

	cmp_ok $errors, '==', $STRICT_IS_POSSIBLE ? 1 : 0,
	    't/data/url_links.pod error count with web checks disabled';
    }
}

foreach my $mi ( Test::Pod::LinkCheck::Lite->new()->module_index() ) {

    local $ENV{HOME} = 't/data';
    local $ENV{PERL5_CPANPLUS_HOME} = 't/data';

    my $t = Test::Pod::LinkCheck::Lite->new(
	module_index	=> $mi,
	strict		=> $STRICT_IS_POSSIBLE,
    );

    note "Test with module_index => $mi";

    $t->pod_file_ok( 't/data/external_uninstalled.pod' );
}

done_testing;

1;

# ex: set textwidth=72 :
