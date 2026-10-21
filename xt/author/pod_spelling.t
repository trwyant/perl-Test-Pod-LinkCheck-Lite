package main;

use strict;
use warnings;

BEGIN {
    eval {
	require Test::Spelling;
	1;
    } or do {
	print "1..0 # skip Test::Spelling not available.\n";
	exit;
    };
    Test::Spelling->import();
}

add_stopwords( <DATA> );

my $lib = -d 'blib' ? 'blib' : 'lib';
all_pod_files_spelling_ok( $lib, qw{ eg tools } );

1;
__DATA__
Alceu
Andreas
Anwar
de
darwin
deponent
dir
freebsd
Freitas
hocery
Kirmess
König
manpath
Mohammed
merchantability
multifold
openbsd
recursed
ReactOS
Rezić
Rodrigues
sayeth
Slaven
VMS
Wyant
XCode
