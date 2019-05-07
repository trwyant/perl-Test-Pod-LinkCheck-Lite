package main;

use strict;
use warnings;

use Test::More 0.88;

require_ok 'Test::Pod::LinkCheck::Lite'
    or BAIL_OUT $@;

my $ms = eval { Test::Pod::LinkCheck::Lite->new() };
isa_ok $ms, 'Test::Pod::LinkCheck::Lite'
    or BAIL_OUT $@;

if ( $ENV{AUTHOR_TESTING} ) {
    note 'The following modules are required for author testing';
    require_ok 'LWP::UserAgent'
	or BAIL_OUT 'LWP::UserAgent required for author testing';
    require_ok 'HTTP::Tiny'
	or BAIL_OUT 'HTTP::Tiny required for author testing';
}

done_testing;

1;
