# (X)Emacs mode: -*- cperl -*-

use strict;

=head1 Unit Test Package for Log::Info

This package tests that Log::Info, with :trap invoked, only logs die()s,
warnings, etc. once to stderr.

=cut

use FindBin 1.42 qw( $Bin );
use Test 1.13 qw( ok plan );

use lib $Bin;
use test2 qw( runcheck );

BEGIN {
  # 1 for compilation test,
  plan tests  => 61,
       todo   => [],
}

# ----------------------------------------------------------------------------

=head2 Test 1: compilation

This test confirms that the test script and the modules it calls compiled
successfully.

=cut

ok 1, 1, 'compilation';

# -------------------------------------

sub death {
  my ($name, $call, $libs, $text, $exit, $type) = @_;
  $type = 0
    unless defined $type;

  my ($out, $err) = ('') x 2;
  ok(runcheck([[$^X, @$libs, '-MLog::Info=:trap', -e => qq'$call "Blibble"'],
               '>', \$out, '2>', \$err,],
              'die ( 1)', undef, $exit),
     1,                                                          "$name ( 1)");
  ok $out, '',                                                   "$name ( 2)";

  my $expect = "$text at -e line 1";
  if ( $type == 1 ) {
    $expect .= ".\n";
  } elsif ( $type == 2 ) {
    $expect = qr/$text (.* )?at -e line 1\n/s;
  } else {
    $expect .= "\n";
  }

  ok $err, $expect,                                              "$name ( 3)";

  ($out, $err) = ('') x 2;
  ok(runcheck([[$^X, @$libs, '-MLog::Info=:trap', -e => qq'$call "Blibble\n"'],
               '>', \$out, '2>', \$err,],
              'die ( 1)', undef, $exit),
     1,                                                          "$name ( 4)");
  ok $out, '',                                                   "$name ( 5)";
  $expect = "$text\n";
  if ( $type == 2 or $name eq 'croak (imported)') {
    # I'm not at all sure why we need to special-case croak, but we do.  
    # So there.
    $expect = qr/$text(.* )?at -e line 1\n/s;
  }
  ok $err, $expect,                                              "$name ( 6)";
}

death('die',  'die', [], 'Blibble', 255);
death('warn', 'warn', [], 'Blibble', 0, 1);
for (qw/ carp cluck confess croak /) {
  my $exit = (($_ eq 'croak' || $_ eq 'confess') ? 255 : 0);
  my $type = ($_ eq 'confess'                    ?   2 : 0);
  death("$_ (imported)",     $_,       ["-MCarp=$_"], 'Blibble', $exit, $type);
  death("$_ (not imported)","Carp::$_",["-MCarp"],    'Blibble', $exit, 0);
}

# ----------------------------------------------------------------------------
