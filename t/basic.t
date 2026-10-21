package main;

use strict;
use warnings;

use Test::More 0.88;

require_ok 'Test::Pod::LinkCheck::Lite::Cache::File'
    or BAIL_OUT $@;

my $obj = eval { Test::Pod::LinkCheck::Lite::Cache::File->new() };
isa_ok $obj, 'Test::Pod::LinkCheck::Lite::Cache::File'
    or BAIL_OUT $@;

require_ok 'Test::Pod::LinkCheck::Lite'
    or BAIL_OUT $@;

my $ms = eval { Test::Pod::LinkCheck::Lite->new() };
isa_ok $ms, 'Test::Pod::LinkCheck::Lite'
    or BAIL_OUT $@;

done_testing;

1;
