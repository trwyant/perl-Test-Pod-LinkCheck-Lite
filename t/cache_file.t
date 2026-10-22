package main;

use 5.008;

use strict;
use warnings;

use Digest::MD5 ();
use File::Temp qw{ tempdir };
use Test::More 0.88;	# Because of done_testing();

use Test::Pod::LinkCheck::Lite::Cache::File;

my $tempdir = tempdir( CLEANUP => 1 );
my $cache = Test::Pod::LinkCheck::Lite::Cache::File->new(
    root_dir	=> $tempdir,
);

# NOTE that at the bottom of this we redefine get_keys() to return a
# reference to a sorted array.
sub digest { return [ sort( map { Digest::MD5::md5_hex( $_ ) } @_ ) ]; }

is_deeply $cache->get_keys(), [], 'Initially empty';

$cache->set( fubar => 'snafu' );
is_deeply $cache->get_keys(), digest( 'fubar' ), "set( 'fubar' )";
is $cache->get( 'fubar' ), 'snafu', "get( 'fubar' )";

$cache->set( answer => 42 );
is_deeply $cache->get_keys(), digest( qw{ answer fubar } ),
    "set( 'answer' )";

$cache->remove( 'fubar' );
is_deeply $cache->get_keys(), digest( 'answer' ), "remove( 'fubar' )";

$cache->expire( 'answer' );
my $obj;
is $cache->get( 'answer', obj_ref => \$obj ), undef, "expire( 'answer' )";
ok $obj, 'Expired entry still exists';
$obj
    and ok $obj->is_expired(), 'Expired entry knows it is expired';
is_deeply $cache->get_keys(), digest( 'answer' ),
    "get_keys() still returns 'answer'";

$cache->purge();
$cache->get( 'answer', obj_ref => \$obj );
ok ! $obj, 'purge()';
is_deeply $cache->get_keys(), [], 'purge()';

$cache->set( Batman => 'Robin' );
is $cache->get( 'Batman' ), 'Robin', "set( 'Batman' )";

$cache->clear();
is_deeply $cache->get_keys(), [], 'clear()';

done_testing;

BEGIN {
    my $get_keys = Test::Pod::LinkCheck::Lite::Cache::File->can(
	'get_keys' );
    no warnings qw{ redefine };
    *Test::Pod::LinkCheck::Lite::Cache::File::get_keys = sub {
	my ( $self ) = @_;
	return [ sort $get_keys->( $self ) ];
    };
}

1;

# ex: set textwidth=72 :
