package Test::Pod::LinkCheck::Lite;

use 5.008;

use strict;
use warnings;

use B::Keywords ();		# Not core
use Carp ();			# Core since 5.0
use File::Find ();		# Core since 5.0
use File::Spec;			# Core since 5.4.5
use IPC::Cmd ();		# Core since 5.9.5
use Pod::Perldoc ();		# Core since 5.8.1
use Pod::Simple::SimpleTree ();	# Not core
use Scalar::Util ();		# Core since 5.7.3
use Test::Builder ();		# Core since 5.6.2

our $VERSION = '0.000_001';

use constant SCALAR_REF	=> ref \0;

# NOTE that new() gets us a singleton. For this reason I use
# $Test::Builder::Level (localized) to get tests reported relative to
# the correct file and line, rather than setting the 'level' attribute.
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
	$dflt{$1} = $code->();
    }

    # For the use of t/pod_file_ok.t ONLY. May be removed without
    # notice. NOTE that if 'strict' is true, links of the form
    # L<text|module/section> will cause failures if the module is not
    # installed.
    sub __strict_is_possible {
	return $dflt{man} && $dflt{ua} && 1;
    }

    sub _init {
	my ( $self, %arg ) = @_;
	foreach my $key ( keys %dflt ) {
	    exists $arg{$key}
		or $arg{$key} = $dflt{$key};
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

{
    my $dflt;
    my $loaded;
    sub _default_ua {
	unless ( $loaded ) {
	    $loaded = 1;
	    foreach my $class ( qw{ HTTP::Tiny LWP::UserAgent } ) {
		( my $fn = "$class.pm" ) =~ s| :: |/|smxg;
		local $@ = undef;
		eval {
		    require $fn;
		    1;
		} or next;
		$dflt = $class;
		return $class;
	    }
	    $TEST->diag(
		'Can not check urls; neither HTTP::Tiny nor LWP::UserAgent available' );
	}
	return $dflt;
    }
}

sub _init_man {
    my ( $self, $name, $value ) = @_;
    $self->{$name} = $value ? 1 : 0;
    return;
}

sub _init_strict {
    my ( $self, $name, $value ) = @_;
    $self->{$name} = $value ? 1 : 0;
    return;
}

sub _init_ua {
    my ( $self, $name, $value ) = @_;
    my $arg = $value;
    if ( defined $value ) {
	unless ( ref $value ) {
	    ( my $fn = "$value.pm" ) =~ s| :: |/|smxg;
	    require $fn;
	    $value = $value->new();
	}
	$value->can( 'head' )
	    or Carp::croak( "$name $arg must support the head() method" );
	$self->{ua} = $value;
    }
    return;
}

sub all_pod_files_ok {
    my ( $self, @dir ) = @_;
    @dir
	or push @dir, 'blib';
    my $errors = 0;
    File::Find::find( {
	    no_chdir	=> 1,
	    wanted	=> sub {
		$self->_is_perl_file( $_ )
		    and $errors += $self->pod_file_ok( $_ );
	    },
	},
	@dir,
    );
    return $errors;
}

sub man {
    my ( $self ) = @_;
    return $self->{man};
}

sub pod_file_ok {
    my ( $self, $file ) = @_;

    delete $self->{_section};

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
	return $self->_fail(
	    "File $file does not exist, or is not a normal file" );
    }

    $parser->any_errata_seen()
	and return $self->_fail( "$file_name contains POD errors" );

    my $msg = "$file_name contains no broken links";

    $self->{_root} = $parser->root();
    my @links = $self->_extract_nodes( \&_want_links )
	or return $self->_pass( $msg );

    my $errors = 0;

    foreach my $link ( @links ) {
	my $code = $self->can( "_handle_$link->[1]{type}" )
	    or Carp::confess( "TODO - link type $link->[1]{type} not supported" );
	$errors += $code->( $self, $file_name, $link );
    }

    return $errors || $self->_pass( $msg );
}

sub strict {
    my ( $self ) = @_;
    return $self->{strict};
}

sub ua {
    my ( $self ) = @_;
    return $self->{ua};
}

sub _pass {
    my ( undef, $msg ) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 2;
    $TEST->ok( 1, $msg );
    return 0;
}

sub _fail {
    my ( undef, $msg ) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 2;
    $TEST->ok( 0, $msg );
    return 1;
}

sub _skip {
    my ( undef, $msg ) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 2;
    $TEST->skip( $msg );
    return 0;
}

sub _strict {
    my ( $self, $msg ) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 2;
    if ( $self->{strict} ) {
	$TEST->ok( 0, $msg );
	return 1;
    } else {
	$TEST->skip( $msg );
	return 0;
    }
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

sub _extract_nodes {
    my ( $self, $want, $node ) = @_;

    $want ||= sub { return $_[1] };
    $node ||= $self->{_root};

    ref $node
	or return;

    return (
	$want->( $self, $node ), map { $self->_extract_nodes(
	    $want, $_ ) } @$node[ 2 .. $#$node ] );
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
    return $self->_strict(
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
		and return $self->_fail(
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
    $self->{_cache}{uninstalled} ||=
	$self->_get_module_index_cpanp() ||
	$self->_get_module_index_cpan() ||
	1;


    ref $self->{_cache}{uninstalled}
	or return $self->_strict(
	"$file_name Link L<$link->[1]{raw}> not checked; " .
	'not found on this system' );

    $self->{_cache}{uninstalled}{$module}
	or return $self->_fail(
	"$file_name link L<$link->[1]{raw}> links to unknown module" );
    $section
	or return 0;
    return $self->_skip(
	"$file_name link L<$link->[1]{raw}> not checked: " .
	'module exists, but unable to check sections of uninstalled modules' );
}

# TODO cpanp, cpm. Probably not cpanm because it is so completely
# self-contained that there appears to be no way to reach into it and
# rummage around inside. CPANPLUS was core starting in 5.9.5, removed in
# 5.19.0. Like CPAN, I need to figure out how to prevent it from
# initializing itself if it is not in fact in use. No idea about
# App::cpm yet.
sub _get_module_index_cpan {
#   my ( $self ) = @_;

    {
	# This is probably pure paranoia, since CPAN is core.
	local $@ = undef;

	eval {
	    require CPAN;
	    require CPAN::HandleConfig;
	    require CPAN::Index;
	    1;
	} or return;
    }

    my @missing_keys;
    my $missing_config_data = \&CPAN::HandleConfig::missing_config_data;

    no warnings qw{ redefine };		## no critic (ProhibitNoWarnings)

    # Steal number of required-but-unspecified configuration items, and
    # (significantly) prevent CPAN from initializing itself if it is not
    # already initialized.
    local *CPAN::HandleConfig::missing_config_data = sub {
	@missing_keys = $missing_config_data->();
	return;
    };

    # Suppress CPAN's informational messages.
    local *STDOUT;
    open STDOUT, '>', File::Spec->devnull();	## no critic (RequireCheckedOpen)

    CPAN::HandleConfig->load();

    @missing_keys
	and return;

    # Note that if there were no missing keys, the fact that we did not
    # return them is immaterial, and the CPAN stuff should work.

    # Cribbed from Test::Pod::LinkCheck

    local $CPAN::Config->{use_sqlite} = 0;
    CPAN::Index->read_metadata_cache();

    return $CPAN::META->{readwrite}{'CPAN::Module'};
}

sub _get_module_index_cpanp {
#   my ( $self ) = @_;

    {
	local $@ = undef;
	local $SIG{__WARN__} = sub {};	# Suppress deprecation messages

	eval {
	    require CPANPLUS::Backend;
	    require CPANPLUS::Internals::Utils;
	    require CPANPLUS::Error;
	    1;
	} or return;
    }

    no warnings qw{ redefine };		## no critic (ProhibitNoWarnings)

    # Steal directory maker

    my @missing_dirs;
    local *CPANPLUS::Internals::Utils::_mkdir = sub {
	my ( undef, %arg ) = @_;
	push @missing_dirs, $arg{dir};
	$@ = 'CPANPLUS initialization is disabled';
	return;
    };

    my $modinx;

    {
	# Local symbol block, so that we get STDERR back after we're
	# done with CPANPLUS, but before we return from the subroutine.

	# Suppress CPANPLUS's informational messages.
	local $CPANPLUS::Error::MSG_FH;
	open $CPANPLUS::Error::MSG_FH, '>',
	    File::Spec->devnull();	## no critic (RequireCheckedOpen)

	# Suppress CPANPLUS's error messages.
	local $CPANPLUS::Error::ERROR_FH;
	open $CPANPLUS::Error::ERROR_FH, '>',
	    File::Spec->devnull();	## no critic (RequireCheckedOpen)

	# Suppress Params::Check's error messages
	local $SIG{__WARN__} = sub {};

	my $cb = CPANPLUS::Backend->new();

	$modinx = $cb->module_tree();

    }

    @missing_dirs
	and return;

    return $modinx;
}

# Handle url links. This is something like L<http://...> or
# L<...|http://...>.
sub _handle_url {
    my ( $self, $file_name, $link ) = @_;

    $self->{ua}
	or return $self->_strict(
	"$file_name link L<$link->[1]{raw}> not checked; url checks disabled" );

    $link->[1]{to}
	or return $self->_fail(
	    "$file_name link L<$link->[1]{raw} contains no url" );

    # NOTE that the URL is actually a Pod::Simple::LinkSection object,
    # which stringifies to the content of the link. This works
    # transparantly with HTTP::Tiny, but not with LWP::UserAgent. So we
    # force stringification.
    my $rslt = $self->{ua}->head( "$link->[1]{to}" );

    my $status_line;
    if ( Scalar::Util::blessed( $rslt ) ) {
	$rslt->is_success()
	    and return 0;
	$status_line = $rslt->status_line();
    } else {
	$rslt->{success}
	    and return 0;
	$status_line = "$rslt->{status} $rslt->{reason}";
    }
    return $self->_fail(
	"$file_name link L<$link->[1]{raw}> broken: $status_line" );
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
    local $_ = <$fh>;
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
    ref $node->[2]
	and return;
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
is done.

This module started its life as a low-dependency version of
L<Test::Pod::LinkCheck|Test::Pod::LinkCheck>. Significant
differences from that module include:

=over

=item Minimal use of the shell

This module shells out only to check C<man> links.

=item Unchecked links are explicitly skipped

That is, a skipped test is generated for each.
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
or whatever). They will only be checked if the C<ua> attribute is true,
and can only be successfully checked if Perl has access to the specified
URL.

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
that can be found. Currently this only works if the C<CPAN> client is
configured. Sections can not be checked.

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

=item man

This Boolean argument is true if C<man> links are to be checked, and
false if not. The default is the value of C<IPC::Cmd::can_run( 'man' )>.
If this returns false a warning is generated, and C<man> links are not
checked.

=item strict

If this Boolean argument is true, links that can not be checked are
considered to be test failures. If it is false, links that can not be
checked are skipped.

This argument is intended primarily for testing, since asserting it may
cause test failures for reasons outside either the user's or the
author's control. For example C<man> links will fail on a system without
a C<man> program, even if they specify valid man pages.

The default is false.

=item ua

This argument is the user agent to be used to check C<url> links, or the
name of the user agent class. The default is C<'HTTP::Tiny'> if that can
be loaded, or C<'LWP::UserAgent'> if that can be loaded. If neither is
present a warning is generated and C<url> links are not checked.

If an actual object is passed, it must support the C<head()> method,
which must return either an L<HTTP::Response|HTTP::Response> or
equivalent object, or a hash containing keys C<{success}>, C<{status}>,
and C<{reason}>.

If you want to disable C<url> checking, specify this argument with value
C<undef>.

=back

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

=head2 man

 $t->man() and say 'man links are checked';

This method returns the value of the C<'man'> attribute.

=head2 pod_file_ok

 my $errors = $t->pod_file_ok( 'lib/Foo/Bar.pm' );

This method tests the links in the given file, returning the number of
errors found. Each error appears in the TAP output as a test failure. If
no errors are found, a passing test will appear in the TAP output.

=head2 strict

This method returns the value of the C<'strict'> attribute.

=head2 ua

This method returns the value of the C<'ua'> attribute. This will be an
actual object even if a class name was specified, or C<undef> if
C<'url'> links are not being checked.

=head1 SEE ALSO

L<Test::Pod::LinkCheck|Test::Pod::LinkCheck>

L<Test::Pod::Links|Test::Pod::Links>

L<Test::Pod::No404s|Test::Pod::No404s>

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
