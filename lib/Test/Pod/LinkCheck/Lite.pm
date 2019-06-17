package Test::Pod::LinkCheck::Lite;

use 5.008;

use strict;
use warnings;

use B::Keywords ();		# Not core
use Carp ();			# Core since 5.0
use File::Find ();		# Core since 5.0
use File::Spec;			# Core since 5.4.5
use HTTP::Tiny;			# Core since 5.13.9
use IPC::Cmd ();		# Core since 5.9.5
use Pod::Perldoc ();		# Core since 5.8.1
use Pod::Simple::LinkSection;	# Core since 5.9.3 (part of Pod::Simple)
use Pod::Simple::SimpleTree ();	# Not core
use Scalar::Util ();		# Core since 5.7.3
use Storable ();		# Core since 5.7.3
use Test::Builder ();		# Core since 5.6.2

our $VERSION = '0.000_900';

use constant ON_DARWIN		=> 'darwin' eq $^O;
use constant ON_VMS		=> 'VMS' eq $^O;

our $DIRECTORY_LEADER;	# FOR TESTING ONLY -- may be retracted without notice
defined $DIRECTORY_LEADER
    or $DIRECTORY_LEADER = ON_VMS ? '_' : '.';

my $DOT_CPAN		= "${DIRECTORY_LEADER}cpan";

use constant ARRAY_REF	=> ref [];
use constant CODE_REF	=> ref sub {};
use constant HASH_REF	=> ref {};
use constant NON_REF	=> ref 0;
use constant REGEXP_REF	=> ref qr<x>smx;
use constant SCALAR_REF	=> ref \0;

# NOTE that Test::Builder->new() gets us a singleton. For this reason I
# use $Test::Builder::Level (localized) to get tests reported relative
# to the correct file and line, rather than setting the 'level'
# attribute.
my $TEST = Test::Builder->new();

sub new {
    my ( $class, %arg ) = @_;
    my $self = bless {}, ref $class || $class;
    return _init( $self, %arg );
}

{
    my %dflt;
    local $_ = undef;
    foreach ( keys %Test::Pod::LinkCheck::Lite:: ) {
	m/ \A _default_ ( .+ ) /smx
	    and my $code = __PACKAGE__->can( $_ )
	    or next;
	$dflt{$1} = $code;
    }

    sub _init {
	my ( $self, %arg ) = @_;
	foreach my $key ( keys %dflt ) {
	    exists $arg{$key}
		or $arg{$key} = $dflt{$key}->();
	}
	foreach my $name ( keys %arg ) {
	    if ( my $code = $self->can( "_init_$name" ) ) {
		$code->( $self, $name, $arg{$name} );
	    } elsif ( defined $arg{$name} ) {
		Carp::croak( "Unknown argument $name" );
	    }
	}
	return $self;
    }
}

sub _default_check_url {
    return 1;
}

sub _default_ignore_url {
    return [];
}

{
    my $checked;
    my $rslt;

    sub _default_man {
	unless ( $checked ) {
	    $checked = 1;
	    $rslt = IPC::Cmd::can_run( 'man' )
		or $TEST->diag(
		q<Can not check man pages; 'man' not installed> );
	}
	return $rslt;
    }
}

sub _default_module_index {
    my @handlers;
    foreach ( keys %Test::Pod::LinkCheck::Lite:: ) {
	m/ \A _get_module_index_ ( .+ ) /smx
	    and __PACKAGE__->can( $_ )
	    or next;
	push @handlers, $1;
    }
    @handlers = sort @handlers;
    return \@handlers;
}

sub _default_agent {
    return HTTP::Tiny->new()->agent();
}

sub _init_check_url {
    my ( $self, $name, $value ) = @_;
    $self->{$name} = $value ? 1 : 0;
    return;
}

{
    my %handler;

    %handler = (
	ARRAY_REF,	sub {
	    my ( $spec, $value ) = @_;
	    $handler{ ref $_ }->( $spec, $_ ) for @{ $value };
	    return;
	},
	CODE_REF,	sub {
	    my ( $spec, $value ) = @_;
	    push @{ $spec->{ CODE_REF() } }, $value;
	    return;
	},
	HASH_REF,	sub {
	    my ( $spec, $value ) = @_;
	    $spec->{ NON_REF() }{$_} = 1 for
		grep { $value->{$_} } keys %{ $value };
	    return;
	},
	NON_REF,	sub {
	    my ( $spec, $value ) = @_;
	    defined $value
		or return;
	    $spec->{ NON_REF() }->{$value} = 1;
	    return;
	},
	REGEXP_REF,	sub {
	    my ( $spec, $value ) = @_;
	    push @{ $spec->{ REGEXP_REF() } }, $value;
	    return;
	},
	SCALAR_REF,	sub {
	    my ( $spec, $value ) = @_;
	    $spec->{ NON_REF() }->{$$value} = 1;
	    return;
	},
    );

    sub _init_ignore_url {
	my ( $self, $name, $value ) = @_;

	my $spec = $self->{$name} = {};
	eval {
	    $handler{ ref $value }->( $spec, $value );
	    1;
	} or Carp::confess(
	    "Invalid ignore_url value '$value': must be scalar, regexp, array ref, or undef" );
	return;
    }
}

sub _init_man {
    my ( $self, $name, $value ) = @_;
    $self->{$name} = $value ? 1 : 0;
    return;
}

sub _init_module_index {
    my ( $self, $name, $value ) = @_;
    my @val = map { split qr{ \s* , \s* }smx } ARRAY_REF eq ref $value ?
    @{ $value } : $value;
    my @handlers;
    foreach my $mi ( @val ) {
	my $code = $self->can( "_get_module_index_$mi" )
	    or Carp::croak( "Invalid module_index value '$mi'" );
	push @handlers, $code;
    }
    $self->{$name} = \@val;
    $self->{"_$name"} = \@handlers;
    return;
}

sub _init_agent {
    my ( $self, $name, $value ) = @_;
    $self->{$name} = $value;
    return;
}

sub _process_array_argument {
    my ( $arg ) = @_;
    ARRAY_REF eq ref $arg
	or $arg = [ $arg ];
    return [ map { split qr< \s*, \s* >smx } @{ $arg } ];
}

sub all_pod_files_ok {
    my ( $self, @dir ) = @_;

    @dir
	or push @dir, 'blib';

    my ( $fail, $pass, $skip ) = ( 0 ) x 3;

    File::Find::find( {
	    no_chdir	=> 1,
	    wanted	=> sub {
		if ( $self->_is_perl_file( $_ ) ) {
		    $TEST->note( "Checking POD links in $File::Find::name" );
		    my ( $f, $p, $s ) = $self->pod_file_ok( $_ );
		    $fail += $f;
		    $pass += $p;
		    $skip += $s;
		}
		return;
	    },
	},
	@dir,
    );
    return wantarray ? ( $fail, $pass, $skip ) : $fail;
}

sub check_url {
    my ( $self ) = @_;
    return $self->{check_url}
}

# This subroutine returns the value of the ignore_url attribute. It is
# PRIVATE to this package, and may be changed or revoked at any time.
# If called with an argument, it returns a true value if that argument
# is a URL that is to be ignored, and false otherwise.
sub __ignore_url {
    my ( $self, $url ) = @_;
    @_ > 1
	or return $self->{ignore_url};
    my $spec = $self->{ignore_url};
    $spec->{ NON_REF() }{$url}
	and return 1;
    foreach my $re ( @{ $spec->{ REGEXP_REF() } } ) {
	$url =~ $re
	    and return 1;
    }
    local $_ = $url;
    foreach my $code ( @{ $spec->{ CODE_REF() } } ) {
	$code->()
	    and return 1;
    }
    return 0;
}

sub man {
    my ( $self ) = @_;
    return $self->{man};
}

sub module_index {
    my ( $self ) = @_;
    wantarray
	and return @{ $self->{module_index} };
    local $" = ',';
    return "@{ $self->{module_index} }";
}

sub pod_file_ok {
    my ( $self, $file ) = @_;

    delete $self->{_section};
    $self->{_test} = {
	pass	=> 0,
	fail	=> 0,
	skip	=> 0,
    };

    my $parser = Pod::Simple::SimpleTree->new();

    my $file_name;
    if ( SCALAR_REF eq ref $file ) {
	$file_name = ${ $file } =~ m/ \n /smx ?
	    "String $file" :
	    "String '${ $file }'";
	$parser->parse_string_document( ${ $file } );
    } elsif ( -f $file ) {
	$file_name = "File $file";
	$parser->parse_file( $file );
    } else {
	$self->_fail(
	    "File $file does not exist, or is not a normal file" );
	return wantarray ? ( 1, 0, 0 ) : 1;
    }

    $parser->any_errata_seen()
	and $TEST->diag( "$file_name contains POD errors" );

    my $msg = "$file_name contains no broken links";

    $self->{_root} = $parser->root();
    my @links = $self->_extract_nodes( \&_want_links )
	or do {
	$self->_pass( $msg );
	return wantarray ? ( 0, 1, 0 ) : 0;
    };

    my $errors = 0;

    foreach my $link ( @links ) {
	my $code = $self->can( "_handle_$link->[1]{type}" )
	    or Carp::confess( "TODO - link type $link->[1]{type} not supported" );
	$errors += $code->( $self, $file_name, $link );
    }

    $errors
	or $self->_pass( $msg );
    return wantarray ?
	( @{ $self->{_test} }{ qw{ fail pass skip } } ) :
	$self->{_test}{fail};
}

sub _user_agent {
    my ( $self ) = @_;
    return( $self->{_user_agent} ||= HTTP::Tiny->new(
	    agent	=> $self->agent(),
	) );
}

sub agent {
    my ( $self ) = @_;
    return $self->{agent};
}

sub _pass {
    my ( $self, @msg ) = @_;
    local $Test::Builder::Level = _nest_depth();
    $TEST->ok( 1, join '', @msg );
    $self->{_test}{pass}++;
    return 0;
}

sub _fail {
    my ( $self, @msg ) = @_;
    local $Test::Builder::Level = _nest_depth();
    $TEST->ok( 0, join '', @msg );
    $self->{_test}{fail}++;
    return 1;
}

sub _skip {
    my ( $self, @msg ) = @_;
    local $Test::Builder::Level =  _nest_depth();
    $TEST->skip( join '', @msg );
    $self->{_test}{skip}++;
    return 0;
}

# Build the section hash. This has a key for each section found in the
# parse tree, with a true value for that key. The return is a reference
# to said hash.
sub _build_section_hash {
    my ( $self, $root ) = @_;
    return {
	map { $_->[2] => 1 }
	$self->_extract_nodes( \&_want_sections, $root ) };
}

# my @nodes = $self->_extract_nodes( $want, $node )
#
# This subroutine extracts all subnodes of the given node that pass the
# test specified by $want. The arguments are:
#
# $want is a code reference called with arguments $self and the subnode
#   being considered. It should return the subnode itself if that is
#   wanted, or a false value otherwise. The default always returns the
#   subnode.
# $node is the node from which subnodes are to be extracted. If
#   unspecified the root of the latest parse will be used. Note that
#   this argument will be returned if it passes the $want check.
#
sub _extract_nodes {
    my ( $self, $want, $node ) = @_;

    $want ||= sub { return $_[1] };
    defined $node
	or $node = $self->{_root};

    ref $node
	or return;

    # The grep() below is paranoia based on the amount of pain incurred
    # in finding that I needed to check for a defined value rather than
    # a true value for the $node argument.
    return (
	$want->( $self, $node ), map { $self->_extract_nodes(
	    $want, $_ ) } grep { defined } @$node[ 2 .. $#$node ] );
}

# Get the information on installed documentation. If the doc is found
# the return is a reference to a hash containing key {file}, value the
# path name to the file containing the documentation. This works both
# for module documentation (whether in the .pm or a separate .pod), or
# regular .pod documentation (e.g. perldelta.pod).
sub _get_installed_doc_info {
    my ( undef, $module ) = @_;
    my $pd = Pod::Perldoc->new();

    # Pod::Perldoc writes to STDERR if the module (or whatever) is not
    # installed, so we localize STDERR and reopen it to the null device.
    # The reopen of STDERR is unchecked because if it fails we still
    # want to run the tests. They just may be noisy.
    local *STDERR;
    open STDERR, '>', File::Spec->devnull();	## no critic (RequireCheckedOpen)

    # NOTE that grand_search_init() is undocumented.
    my ( $path ) = $pd->grand_search_init( [ $module ] );

    close STDERR;

    defined $path
	and return {
	file	=> $path,
    };

    return;
}

# POD link handlers

# Handle a 'man' link.
sub _handle_man {
    my ( $self, $file_name, $link ) = @_;
    my $rslt = $self->_is_man_page( $link )
	and return 0;
    defined $rslt
	and return $self->_fail(
	    "$file_name link L<$link->[1]{raw}> refers to unknown man page" );
    return $self->_skip(
	"$file_name link L<$link->[1]{raw}> not checked; man checks disabled" );
}

# Handle pod links. This is pretty much everything, except for 'man'
# (see above) or 'url' (see below).
sub _handle_pod {
    my ( $self, $file_name, $link ) = @_;

    if ( $link->[1]{to} ) {
	return $self->_check_external_pod_info( $file_name, $link )

    } elsif ( my $section = $link->[1]{section} ) {
	# Internal links (no {to})
	$self->{_section} ||= $self->_build_section_hash();
	$self->{_section}{$section}
	    and return 0;
	return $self->_fail(
	    "$file_name link L<$link->[1]{raw}> links to unknown section" );

    } else {
	# Links to nowhere: L<...|> or L<...|/>
	return $self->_fail(
	    "$file_name link L<$link->[1]{raw}> links to nothing" );
    }
    return 0;
}

sub _check_external_pod_info {
    my ( $self, $file_name, $link ) = @_;
    my $module = $link->[1]{to};
    my $section = $link->[1]{section};

    # If there is no section info it might be a Perl builtin. Return
    # success if it is.
    unless ( $section ) {
	$self->_is_perl_function( $module )
	    and return 0;
    }

    # If it is installed, handle it
    if ( my $data = $self->{_cache}{installed}{$module} ||=
	$self->_get_installed_doc_info( $module ) ) {

	# If it is in fact an installed module AND there is no section,
	# we can return success.
	$section
	    or return 0;

	# Find and parse the section info if needed.
	unless ( $data->{section} ) {

	    my $parser = Pod::Simple::SimpleTree->new();
	    $parser->parse_file( $data->{file} );
	    $parser->any_errata_seen()
		and $TEST->diag(
		"File $data->{file} contains POD errors" );
	    $data->{section} = $self->_build_section_hash(
		$parser->root() );
	}

	$data->{section}{$section}
	    and return 0;

	return $self->_fail(
	    "$file_name link L<$link->[1]{raw}> links to unknown section" );
    }

    # If there is no section, it might be a man page, even though the
    # parser did not parse it as one
    unless ( $section ) {
	$self->_is_man_page( $module )
	    and return 0;
    }

    # It's not installed on this system, but it may be out there
    # somewhere

    $self->{_cache}{uninstalled} ||= $self->_get_module_index();

    return $self->{_cache}{uninstalled}->( $self, $file_name, $link );

}

sub _get_module_index {
    my ( $self ) = @_;
    my @inxes = sort { $a->[1] <=> $b->[1] }
	map { $_->( $self ) } @{ $self->{_module_index} };
    if ( @inxes ) {
	my $modinx = $inxes[-1][0];
	return sub {
	    my ( $self, $file_name, $link ) = @_;
	    my $module = $link->[1]{to};
	    $modinx->( $module )
		or return $self->_fail(
		"$file_name link L<$link->[1]{raw}> links to unknown module" );
	    $link->[1]{section}
		or return 0;
	    return $self->_skip(
		"$file_name link L<$link->[1]{raw}> not checked: " .
		'module exists, but unable to check sections of ',
		'uninstalled modules' );
	};
    } else {
	return sub {
	    my ( $self, $file_name, $link ) = @_;
	    return $self->_skip(
		"$file_name link L<$link->[1]{raw}> not checked; " .
		'not found on this system' );
	};
    }
}

# In all of the module index getters, the return is either nothing at
# all (for inability to use this indexing mechanism) or a refererence to
# to an array. Element [0] of the array is a reference a piece of code
# that takes the module name as its only argument, and returns a true
# value if that module exists and a false value otherwise. Element [1]
# of the array is a Perl time that is characteristic of the information
# in the index (typically the revision date of the underlying file if
# that's the way the index works).

# NOTE that Test::Pod::LinkCheck loads CPAN and then messes with it to
# try to prevent it from initializing itself. After trying this and
# thinking about it, I decided to go after the metadata directly.
sub _get_module_index_cpan {
#   my ( $self ) = @_;

    # The following code reproduces
    # CPAN::HandleConfig::cpan_home_dir_candidates()
    my @dir_list;

    if ( _has_usable( 'File::HomeDir', 0.52 ) ) {
	ON_DARWIN
	    or push @dir_list, File::HomeDir->my_data();
	push @dir_list, File::HomeDir->my_home();
    }

    $ENV{HOME}
	and push @dir_list, $ENV{HOME};
    $ENV{HOMEDRIVE}
	and $ENV{HOMEPATH}
	and push @dir_list, File::Spec->catpath( $ENV{HOMEDRIVE},
	$ENV{HOMEPATH} );
    $ENV{USERPROFILE}
	and push @dir_list, $ENV{USERPROFILE};
    $ENV{'SYS$LOGIN'}
	and push @dir_list, $ENV{'SYS$LOGIN'};

    # The preceding code reproduces
    # CPAN::HandleConfig::cpan_home_dir_candidates()

    foreach my $dir ( @dir_list ) {
	defined $dir
	    or next;
	my $path = File::Spec->catfile( $dir, $DOT_CPAN, 'Metadata' );
	-e $path
	    or next;
	my $rev = ( stat _ )[9];
	my $hash = Storable::retrieve( $path )
	    or return;
	$hash = $hash->{'CPAN::Module'};
	return [
	    sub { return $hash->{$_[0]} },
	    $rev,
	];
    }

    return;
}

sub _get_module_index_cpan_meta_db {
    my ( $self ) = @_;

    my $user_agent = $self->_user_agent();

    my %hash;

    return [
	sub {
	    exists $hash{$_[0]}
		and return $hash{$_[0]};
	    my $resp = $user_agent->head(
		"https://cpanmetadb.plackperl.org/v1.0/package/$_[0]" );
	    return ( $hash{$_[0]} = $resp->{success} );
	},
	time - 86400 * 7,
    ];
}

# Handle url links. This is something like L<http://...> or
# L<...|http://...>.
sub _handle_url {
    my ( $self, $file_name, $link ) = @_;


    $self->check_url()
	or return $self->_skip(
	"$file_name link L<$link->[1]{raw}> not checked; url checks disabled" );

    my $user_agent = $self->_user_agent();

    my $url = "$link->[1]{to}"	# Stringify object
	or return $self->_fail(
	    "$file_name link L<$link->[1]{raw} contains no url" );

    $self->__ignore_url( $url )
	and return $self->_skip(
	"$file_name link L<$link->[1]{raw}> not checked; explicitly ignored" );

    my $resp = $user_agent->head( $url );

    $resp->{success}
	and return 0;

    return $self->_fail(
	"$file_name link L<$link->[1]{raw}> broken: ",
	"$resp->{status} $resp->{reason}",
    );
}

{
    my %checked;

    sub _has_usable {
	my ( $module, $version ) = @_;

	unless ( exists $checked{$module} ) {
	    local $@ = undef;
	    ( my $fn = "$module.pm" ) =~ s| :: |/|smxg;
	    eval {
		require $fn;
		$checked{$module} = 1;
		1;
	    } or do {
		$checked{$module} = 0;
	    };
	}

	$checked{$module}
	    or return;

	if ( defined $version ) {
	    my $rslt = 1;
	    local $SIG{__DIE__} = sub { $rslt = undef };
	    $module->VERSION( $version );
	    return $rslt;
	}

	return 1;
    }
}

sub _is_man_page {
    my ( $self, $link ) = @_;
    $self->{man}
	or return;
    $link->[1]{to}
	or return;
    my ( $page, $sect ) = $link->[1]{to} =~ m/ ( [^(]+ ) (?: [(] ( [^)]+ ) [)] )? /smx
	or return;
    $page =~ s/ \s+ \z //smx;
    my @pg = (
	$sect ? $sect : (),
	$page,
    );
    return $self->{_cache}{man}{"@pg"} ||= IPC::Cmd::run( COMMAND => [
	    qw{ man -w }, @pg ] );
}

sub _is_perl_file {
    my ( undef, $path ) = @_;
    -e $path
	and -T _
	or return;
    $path =~ m/ [.] (?: (?i: pl ) | pm | t ) /smx
	and return 1;
    open my $fh, '<', $path
	or return;
    local $_ = <$fh> || '';
    close $fh;
    return m/ perl /smx;
}

{
    my $bareword;

    sub _is_perl_function {
	my ( undef, $word ) = @_;
	$bareword ||= {
	    map { $_ => 1 } @B::Keywords::Functions, @B::Keywords::Barewords };
	return $bareword->{$word};
    }
}

{
    my %ignore;
    BEGIN {
	%ignore = map { $_ => 1 } __PACKAGE__, qw{ DB File::Find };
    }

    sub _nest_depth {
	my $nest = 0;
	$nest++ while $ignore{ caller( $nest ) || '' };
	return $nest;
    }
}

sub _want_links {
    my ( undef, $node ) = @_;
    'L' eq $node->[0]
	or return;
    return $node;
}

sub _want_sections {
    my ( undef, $node ) = @_;
    $node->[0] =~ m/ \A head \d+ \z /smx
	or return;

    # The following rigamarole is to flatten section names that contain
    # formatting codes (e.g. 'foo(I<bar>)'). The formatting codes get
    # lost in the process, but it's how the links themselves come out of
    # the parse, so if we're wrong we are at least consistent.
    my $ls = Pod::Simple::LinkSection->new(
	@{ $node }[ 2 .. $#$node ] );
    @{ $node }[ 2 .. $#$node ] = "$ls";

    return $node;
}

1;

__END__

=head1 NAME

Test::Pod::LinkCheck::Lite - Test POD links

=head1 SYNOPSIS

 use Test::More 0.88;   # for done_testing();
 use Test::Pod::LinkCheck::Lite;
 
 my $t = Test::Pod::LinkCheck::Lite->new();
 $t->all_pod_files_ok();

 done_testing;

=head1 DESCRIPTION

This Perl module tests POD links. A given file generates one failure for
each broken link found. If no broken links are found, one passing test
is generated. This all means that you can not know how many tests will
be generated, and you will need to use L<Test::More|Test::More>'s
C<done_testing()> at the end of your test.

This module should probably be considered alpha-quality code at this
point. It correctly checks most of my modest corpus, but beyond that
deponent sayeth not.

This module started its life as a low-dependency version of
L<Test::Pod::LinkCheck|Test::Pod::LinkCheck>. Significant
differences from that module include:

=over

=item Minimal use of the shell

This module shells out only to check C<man> links.

=item Unchecked links are explicitly skipped

That is, a skipped test is generated for each. Note that
L<Test::Pod::LinkCheck|Test::Pod::LinkCheck> appears to fail the link in
at least some such cases.

=item URL links are checked

This seemed to be an easy enough addition.

=item Dependencies are minimized

Given at least Perl 5.13.9, the only non-core modules used are
L<B::Keywords|B::Keywords> and
L<Pod::Simple::SimpleTree|Pod::Simple::SimpleTree>.

=back

POD links come in the following flavors:

=over

=item * man

These links are of the form C<< LE<lt>manpage (section)E<gt> >>. They
will only be checked if the C<man> attribute is true, and can only be
successfully checked if the C<man> command actually displays man pages,
and C<man -w> can be executed.

=item * url

These links are of the form C<< LE<lt>http://...E<gt> >> (or C<https:>
or whatever). They will only be checked if the C<check_url> attribute is
true, and can only be successfully checked if Perl has access to the
specified URL.

=item * pod (internal)

These links are of the form C<< LE<lt>text|/sectionE<gt> >>. They are
checked using the parse tree in which the link was found.

=item * pod (external)

This is pretty much everything else. There are a number of cases, and
the only way to distinguish them is to run through them.

=over

=item Perl built-ins

These links are of the form C<< LE<lt>text|builtin>E<gt> >> or
C<< LE<lt>builtinE<gt> >>, and are checked against the lists in
L<B::Keywords|B::Keywords>.

=item Installed modules and pod files

These are resolved to a file using L<Pod::Perldoc|Pod::Perldoc>. If a
section was specified, the file is parsed to determine whether the
section name is valid.

=item Uninstalled modules

These are checked against F<modules/02packages.details.txt.gz>, provided
that (or some reasonable facsimile) can be found. Currently we can look
for this information in the following places:

=over

=item File F<Metadata> in the directory used by the C<CPAN> client;

=item Website L<https://cpanmetadb.plackperl.org/>, a.k.a. the CPAN Meta
DB.

=back

If more than one of these is configured (by default they all are), we
look in the newest one.

Sections can not be checked. If a link to a valid (but uninstalled)
module has a section, a skipped test is generated.

=back

=back

The C<::Lite> refers to the fact that a real effort has been made to
reduce non-core dependencies. Under Perl 5.14 and up, the only known
non-core dependencies are L<B::Keywords|B::Keywords> and
L<Pod::Simple::SimpleTree|Pod::Simple::SimpleTree>.

An effort has also been made to minimize the spawning of system
commands.

=head1 METHODS

This class supports the following public methods:

=head2 new

 my $t = Test::Pod::LinkCheck::Lite->new();

This static method instantiates an object. Optional arguments are passed
as name/value pairs.

The following arguments are supported:

=over

=item agent

This argument is the user agent string to use for web access.

The default is that of L<HTTP::Tiny|HTTP::Tiny>.

=item check_url

This Boolean argument is true if C<url> links are to be checked, and
false if not. The default is true.

=item ignore_url

This argument specifies one or more URLs to ignore when checking C<url>
links. It can be specified as:

=over

=item A C<Regexp> object

Any URL that matches this Regexp is ignored.

=item C<undef>

No URLs are ignored.

=item a scalar

This URL is ignored.

=item a scalar reference

The URL referred to is ignored.

=item a hash reference

The URL is ignored if the hash contains a true value for the URL.

=item a code reference

The code is called with the URL to ignore in the topic variable (a.k.a.
C<$_>). The URL is ignored if the code returns a true value.

=item an C<ARRAY> reference

The array can contain any legal ignore specification, and any URL that
matches any value in the array is ignored. Nested arrays are flattened.

=back

The default is C<[]>.

B<Note> that the order in which the individual checks are made is
B<undefined>. OK, the implementation is deterministic, but the order of
evaluation is an implementation detail that the author reserves the
right to change without warning.

=item man

This Boolean argument is true if C<man> links are to be checked, and
false if not. The default is the value of C<IPC::Cmd::can_run( 'man' )>.
If this returns false a warning is generated, and C<man> links are not
checked.

=item module_index

This argument specifies a list of module indices to consult, as either a
comma-delimited string or an array reference. Even if specified a given
index will only be used if it is actually available for use. If more
than one index is found, the most-recently-updated index will be used.
Possible indices are:

=over

=item cpan

Use the module index found in the L<CPAN|CPAN> working directory.

=item cpan_meta_db

Use the CPAN Meta database. Because this is an on-line index it is
considered to be current, but its as-of time is offset to favor local
indices.

=back

By default all indices are considered.

=back

=head2 agent

This method returns the value of the C<'agent'> attribute.

=head2 all_pod_files_ok

 $t->all_pod_files_ok();

This method takes as its arguments the names of one or more files, and
tests any such that are deemed to be Perl files. Directories are
recursed into.

Perl files are considered to be all text files whose names end in
F<.pod>, F<.pm>, or F<.PL>, plus any text files with a shebang line
containing C<'perl'>. File name suffixes are case-sensitive except for
F<.PL>.

If no arguments are specified, the contents of F<blib/> are tested. This
is the recommended usage.

If called in scalar context, this method returns the number of test
failures encountered. If called in list context it return the number of
failures, passes, and skipped tests, in that order.

=head2 check_url

 $t->check_url() and say 'URL links are checked';

This method returns the value of the C<'check_url'> attribute.

=head2 man

 $t->man() and say 'man links are checked';

This method returns the value of the C<'man'> attribute.

=head2 module_index

 say 'Module indices: ', join ', ', $self->module_index();

This method returns the value of the C<'module_index'> attribute. If
called in scalar context it returns a comma-delimited string.

=head2 pod_file_ok

 my $failures = $t->pod_file_ok( 'lib/Foo/Bar.pm' );

This method tests the links in the given file. Each failure appears in
the TAP output as a test failure. If no failures are found, a passing
test will appear in the TAP output.

If called in scalar context, this method returns the number of test
failures encountered. If called in list context it return the number of
failures, passes, and skipped tests, in that order.

=head1 SEE ALSO

L<Test::Pod::LinkCheck|Test::Pod::LinkCheck> by Apocalypse checks all
POD links except for URLs. It is L<Moose|Moose>-based.

L<Test::Pod::Links|Test::Pod::Links> by Sven Kirmess checks all URLs or
URL-like things in the document, whether or not they are actual POD
links.

L<Test::Pod::No404s|Test::Pod::No404s> by Apocalypse checks URL POD
links.

=head1 SUPPORT

Support is by the author. Please file bug reports at
L<http://rt.cpan.org>, or in electronic mail to the author.

=head1 AUTHOR

Thomas R. Wyant, III F<wyant at cpan dot org>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2019 by Thomas R. Wyant, III

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl 5.10.0. For more details, see the full text
of the licenses in the directory LICENSES.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.

=cut

# ex: set textwidth=72 :
