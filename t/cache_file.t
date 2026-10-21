package main;

use 5.008;

use strict;
use warnings;

use File::Temp qw{ tempdir };
use Test::More 0.88;	# Because of done_testing();

use Test::Pod::LinkCheck::Lite::Cache::File;

my $tempdir = tempdir( CLEANUP => 1 );
my $cache = Test::Pod::LinkCheck::Lite::Cache::File->new(
    root_dir	=> $tempdir,
);

is_deeply [ $cache->get_keys() ], [], 'Initially empty';

$cache->set( fubar => 'snafu' );
is_deeply [ $cache->get_keys() ], [ 'fubar' ], "set( 'fubar' )";
is scalar $cache->get( 'fubar' ), 'snafu', "get( 'fubar' )";

$cache->set( answer => 42 );
is_deeply [ sort $cache->get_keys() ], [ qw{ answer fubar } ],
    "set( 'answer' )";

$cache->remove( 'fubar' );
is_deeply [ $cache->get_keys() ], [ 'answer' ], "remove( 'fubar' )";

$cache->expire( 'answer' );
is scalar $cache->get( 'answer' ), undef, "expire( 'answer' )";
is scalar $cache->get_extant( 'answer' ), 42, 'Expired entry still exists';
is_deeply [ $cache->get_keys() ], [], 'Cache should now be empty';
is_deeply [ $cache->get_extant_keys() ], [ 'answer' ],
    "but get_extant_keys() still finds the expired entry";

$cache->purge();
is scalar $cache->get_extant( 'answer' ), undef, 'purge()';
is_deeply [ $cache->get_extant_keys() ], [],
    "and even get_extant_keys() returns no keys";

$cache->set( Batman => 'Robin' );
is scalar $cache->get( 'Batman' ), 'Robin', "set( 'Batman' )";

$cache->clear();
is_deeply [ $cache->get_keys() ], [], 'clear()';

done_testing;

1;

# ex: set textwidth=72 :
