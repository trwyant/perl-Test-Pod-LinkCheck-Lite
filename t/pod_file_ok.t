package main;

use 5.008;

use strict;
use warnings;

use Module::Load::Conditional qw{ check_install };
use Test::More 0.88;	# Because of done_testing();

# This mess is because if Devel::Hide or Test::Without::Module is
# specified on the command line or in an enclosing file, a straight
# 'use lib qw{ inc/Mock }' would trump it, and the mocked modules would
# still be loaded. With this mess, the command-line version is
# $ perl -Mlib=inc/Mock -MDevel::Hide=HTTP::Tiny ...,
# and the 'use if' sees inc/Mock already in @INC and does not add it
# again.  'use if' is core as of 5.6.2, so I should be OK unless I run
# into some Linux packager who knows better than the Perl Porters what
# should be in core (and yes, they exist).

use constant CODE_REF	=> ref sub {};
use constant NON_REF	=> ref 0;
use constant REGEXP_REF	=> ref qr{};

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
}

{
    my $t = Test::Pod::LinkCheck::Lite->new();

    diag '';
    diag q<Default 'check_external_sections' is >,
	Boolean( $t->check_external_sections() );
    diag q<Default 'check_url' is >, Boolean( $t->check_url() );
    diag q<Default 'ignore_url' is >, explain scalar $t->ignore_url();
    diag q<Default 'man' is >, Boolean( $t->man() );
    diag q<Default 'module_index' is ( >, join( ', ', map { "'$_'" }
	$t->module_index() ), ' )';
    diag q<Default 'require_installed' is >,
	Boolean( $t->require_installed() );

    # Encapsulation violation for testing purposes. DO NOT try this at
    # home.
    $t->{_file_name} = 'File fu.bar';

    is $t->__build_test_msg( '1, 2, 3' ), 'File fu.bar 1, 2, 3',
	'Build test message';

    is $t->__build_test_msg( [ undef, {
		raw => 'Bazzle',
	    },
	], 'checked' ),
	'File fu.bar link L<Bazzle> checked',
	'Test message with link';

    is $t->__build_test_msg( [ undef, {
		line_number	=> 42,
		raw		=> 'Bazzle',
	    },
	], 'checked' ),
	'File fu.bar line 42 link L<Bazzle> checked',
	'Test message with line number and link';
}

{
    local $ENV{HOME} = 't/data';

    my $t = Test::Pod::LinkCheck::Lite->new();

    my @rslt;

    $t->pod_file_ok( \'' );

    {
	my ( $fail, $pass, $skip );

	TODO: {
	    local $TODO = 'Deliberate failure';
	    ( $fail, $pass, $skip ) = $t->pod_file_ok( 't/data/nonexistent.pod' );
	}
	cmp_ok $fail, '==', 1,
	'Got expected failure checking non-existent file'
	    or diag "Fail = $fail; pass = $pass; skip = $skip";
    }

    $t->pod_file_ok( 't/data/empty.pod' );

    $t->pod_file_ok( 't/data/no_links.pod' );

    @rslt = $t->pod_file_ok( 't/data/url_links.pod' );
    is_deeply \@rslt, [ 0, 1, 0 ],
	'Test of t/data/url_links.pod returned proper data';

    SKIP: {
	$t->man()
	    or skip 'This system does not support the testing of man links', 1;

	my ( $fail, $pass, $skip );

	( $fail, $pass, $skip ) = $t->pod_file_ok( 't/data/man.pod' )
	    or do {
	    diag "Fail = $fail; pass = $pass; skip = $skip";
	    # TODO ditch the following once I have sorted out the test
	    # failure
	    diag 'Links found: ', explain $t->{_links};
	};
    }

    $t->pod_file_ok( 't/data/internal.pod' );

    # This circumlocution will be used for tests where errors are
    # expected.  Unfortunately it only tests that the correct number of
    # errors are reported, not that the errors reported are the correct
    # ones.

    {
	my ( $fail, $pass, $skip );

	TODO: {
	    local $TODO = 'Deliberate test failures.';
	    ( $fail, $pass, $skip ) = $t->pod_file_ok(
		't/data/internal_error.pod' );
	}

	cmp_ok $fail, '==', 2, 't/data/internal_error.pod had 2 errors'
	    or diag "Fail = $fail; pass = $pass; skip = $skip";
    }

    $t->pod_file_ok( 't/data/external_builtin.pod' );

    $t->pod_file_ok( 't/data/external_installed.pod' );

    SKIP: {

	my $version = 1.40;
	my $rv;
	$rv = check_install(
	    module	=> 'Scalar::Util',
	    version	=> $version,
	) and defined $rv->{version}
	    and $rv->{version} ge $version
	    or skip
	    "External section check needs Scalar::Util version $version", 1;

	$t->pod_file_ok( 't/data/external_installed_section.pod' );

    }

    {
	my ( $fail, $pass, $skip );

	TODO: {
	    local $TODO = 'Deliberate test failures.';
	    ( $fail, $pass, $skip ) = $t->pod_file_ok(
		't/data/external_installed_bad_section.pod' );
	}

	cmp_ok $fail, '==', 1,
	    't/data/external_installed_bad_section.pod had 1 error'
	    or diag "Fail = $fail; pass = $pass; skip = $skip";
    }

    $t->pod_file_ok( 't/data/external_installed_pod.pod' );

    $t->pod_file_ok( 't/data/external_uninstalled.pod' );

    $t->pod_file_ok( 't/data/bug_leading_format_code.pod' );

    $t->pod_file_ok( 't/data/bug_recursion.pod' );

    $t->all_pod_files_ok( 't/data' );

}

{
    my $t = Test::Pod::LinkCheck::Lite->new(
	check_external_sections	=> 0,
    );

    note 'The following test should pass because check_external_sections => 0';
    $t->pod_file_ok(
	't/data/external_installed_bad_section.pod' );

}

{
    my $t = Test::Pod::LinkCheck::Lite->new(
	require_installed	=> 1,
    );

    my ( $fail, $pass, $skip );

    TODO: {
	note 'The following test should fail because require_installed => 1';
	local $TODO = 'Deliberate test failure.';
	( $fail, $pass, $skip ) = $t->pod_file_ok(
	    't/data/external_uninstalled.pod' );
    }

    cmp_ok $fail, '==', 1,
    't/data/external_uninstalled.pod fails without uninstalled module checking'
	or do {
	diag "Fail = $fail; pass = $pass; skip = $skip";
	# TODO ditch the following once I have sorted out the test
	# failure
	diag 'Links found: ', explain $t->{_links};
    };
}

foreach my $check_url ( 0, 1 ) {
    my $t = Test::Pod::LinkCheck::Lite->new(
	check_url	=> $check_url,
    );

    note "Test with explicitly-specified check_url => $check_url";

    if ( $check_url ) {
	$t->pod_file_ok( 't/data/url_links.pod' );
    } else {
	my $errors = $t->pod_file_ok(
	    't/data/url_links.pod' );

	cmp_ok $errors, '==', 0,
	    't/data/url_links.pod error count with url checks disabled';
    }
}

{
    my $code = sub { 0 };

    foreach my $ignore (
	[ []	=> {} ],
	[ undef,	   {} ],
	[ 'http://foo.bar/'	=> {
		NON_REF,	{
		    'http://foo.bar/'	=> 1,
		},
	    },
	],
	[ qr< \Q//foo.bar\E \b >smxi	=> {
		REGEXP_REF,	[
		    qr< \Q//foo.bar\E \b >smxi,
		],
	    },
	],
	[ [ undef, qw< http://foo.bar/ http://baz.burfle/ >, qr|//buzz/| ]	=> {
		NON_REF,	{
		    'http://foo.bar/'	=> 1,
		    'http://baz.burfle/'	=> 1,
		},
		REGEXP_REF,	[
		    qr|//buzz/|,
		],
	    },
	],
	[ [ $code, { 'http://foo/' => 1, 'http://bar/' => 0 } ]	=> {
		NON_REF,	{
		    'http://foo/'	=> 1,
		},
		CODE_REF,	[ $code ],
	    }
	],
    ) {
	my $t = Test::Pod::LinkCheck::Lite->new(
	    ignore_url	=> $ignore->[0],
	);

	is_deeply $t->__ignore_url(), $ignore->[1], join( ' ',
	    'Properly interpreted ignore_url => ',
	    defined $ignore->[0] ? explain $ignore->[0] : 'undef',
	);
    }
}

{
    my $t = Test::Pod::LinkCheck::Lite->new(
	ignore_url	=> qr< \Q//metacpan.org/\E >smx,
    );

    my @rslt = $t->pod_file_ok( 't/data/url_links.pod' );
    is_deeply \@rslt, [ 0, 1, 1 ],
	'Test of t/data/url_links.pod returned proper data when ignoring URL';
}

foreach my $mi ( Test::Pod::LinkCheck::Lite->new()->module_index() ) {

    local $ENV{HOME} = 't/data';

    my $t = Test::Pod::LinkCheck::Lite->new(
	module_index	=> $mi,
    );

    note "Test with module_index => $mi";

    $t->pod_file_ok( 't/data/external_uninstalled.pod' );
}

done_testing;

sub Boolean {
    my ( $arg ) = @_;
    return $arg ? 'true' : 'false';
}

1;

# ex: set textwidth=72 :
