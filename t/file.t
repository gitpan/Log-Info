# (X)Emacs mode: -*- cperl -*-

use strict;

=head1 Unit Test Package for Log::Info functions

This package tests the file-writing functionality of Log::Info

=cut

use Fatal                 qw( close open read seek );
use Fcntl                 qw( SEEK_END );
use File::Glob            qw( );
use IO::Select            qw( );
use POSIX                 qw( tmpnam );
use Test                  qw( ok plan );

# Channel names for playing with
use constant TESTCHAN1 => 'testchan1';
use constant TESTCHAN2 => 'testchan2';

# Sink names for playing with
use constant SINK1 => 'sink1';
use constant SINK2 => 'sink2';

# Message texts for playing with
# Tests rely on no "\n" in these
# Each message to be distinct for searching
use constant MESSAGE1   => 'Cuthbert';
use constant MESSAGE2   => 'Dibble';
use constant MAXMESSLEN => ((length(MESSAGE1) > length(MESSAGE2)) ?
                            length(MESSAGE1) : length(MESSAGE2));

# File sizes for playing with
use constant MAXSIZE1 => 100;
use constant MAXSIZE2 => 80;

use constant MAXMAXSIZE => ((MAXSIZE1 > MAXSIZE2) ? MAXSIZE1 : MAXSIZE2);

# Translators
# TRANS1 adds 2 chars onto each message
# TRANS2 doubles the length of each message
# Each translator leaves the original message in place for searchability
#   (just add to 'em)
use constant TRANS1 => sub { "++$_[0]" };
use constant TRANS2 => sub { scalar(reverse($_[0])) . $_[0] };

use constant TMPNAM1 => tmpnam;
use constant TMPNAM2 => tmpnam;

BEGIN {
  plan tests  => 21;
       todo   => [],
       ;
}

END {
  unlink map glob("$_*"), TMPNAM1, TMPNAM2;
}

use Log::Info qw( :DEFAULT :log_levels );

=head2 Test 1: compilation

This test confirms that the test script and the modules it calls compiled
successfully.

The C<:DEFAULT> and C<:log_levels> tags are passed to the C<use> call for
C<Log::Info>.

=cut

ok 1, 1, 'compilation';

=head2 Test 2: set up pipe to fh

create a pipe from C<$out> to C<$in>.  Unbuffer $out.

Create a channel TESTCHAN1 with sink SINK1 connected to $out at channel level
3, sink level undef.

Test no exception thrown.

=cut

my ($in, $out);

{
  my $ok = 0;
  eval {
    pipe $in, $out
      or die "Pipe failed: $!\n";
    select((select($out), $| = 1)[0]);
    Log::Info::add_channel (TESTCHAN1, 3);
    Log::Info::add_sink    (TESTCHAN1, SINK1, 'FH', undef, { fh => $out });
    $ok = 1;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'set up pipe to fh';
}

=head2 Test 3: log to fh

Log MESSAGE1 at level 4, MESSAGE2 at level 3.

Test that MESSAGE2 only is written, and a newline is appended (and no
exception is thrown).

=cut

{
  my $ok = 0;
  my $read;

  eval {
    Log(TESTCHAN1, 4, MESSAGE1);
    Log(TESTCHAN1, 3, MESSAGE2);

    local $/ = "\n";
    local $SIG{ALRM} = sub { die "Timed out reading from pipe\n" }; alarm 2;
    $read = <$in>;
    alarm 0;
#    sysread $in, $read, 7;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $read, (MESSAGE2 . "\n"), 'log to fh';
}

=head2 Test 4: set up sink to file

Create a channel TESTCHAN2 with sink SINK2 connected to a temporary file.

Test no exception thrown.

=cut

{
  my $ok = 0;
  eval {
    Log::Info::add_channel (TESTCHAN2);
    Log::Info::add_sink    (TESTCHAN2, SINK2, 'FILE', undef,
                            { fn => TMPNAM1 });
    $ok = 1;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'set up sink to file';
}

=head2 Test 5: log to file

Write a log to temporary file.

Test log written.

=cut

{
  my $read;

  eval {
    Log(TESTCHAN2, 4, MESSAGE1);

    open *TMPFH, TMPNAM1;
    local $/ = undef;
    $read = <TMPFH>;
    close *TMPFH;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
  }

  ok $read, (MESSAGE1 . "\n"), 'log to file';
}

=head2 Test 6: add filelog to TESTCHAN1

Add FILE F<tmpnam> log to TESTCHAN1 as SINK2 at level 2.
Log MESSAGE1 at level 1, MESSAGE2 at level 3 to TESTCHAN1.

Test no exception thrown.

=cut

{
  my $ok = 0;
  eval {
    Log::Info::add_sink (TESTCHAN1, SINK2, 'FILE', 2,
                         { fn => TMPNAM1 });
    Log(TESTCHAN1, 1, MESSAGE1);
    Log(TESTCHAN1, 3, MESSAGE2);

    $ok = 1;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'set up sink to file';
}

=head2 Test 7: dual log to fh (1)

Test MESSAGE1 logged to SINK1.

=cut

{
  my $ok = 0;
  my $read;

  eval {
    local $/ = "\n";
    local $SIG{ALRM} = sub { die "Timed out reading from pipe\n" }; alarm 2;
    $read = <$in>;
    alarm 0;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $read, (MESSAGE1 . "\n"), 'dual log to fh (1)';
}

=head2 Test 8: dual log to fh (2)

Test MESSAGE2 logged to SINK1.

=cut

{
  my $ok = 0;
  my $read;

  eval {
    local $/ = "\n";
    local $SIG{ALRM} = sub { die "Timed out reading from pipe\n" }; alarm 2;
    $read = <$in>;
    alarm 0;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $read, (MESSAGE2 . "\n"), 'dual log to fh (2)';
}

=head2 Test 9: dual log to file

Test MESSAGE1 logged to file after MESSAGE1 logged by test 5.

=cut

{
  my $ok = 0;
  my $read;

  eval {
    open *TMPFH, TMPNAM1;
    local $/ = undef;
    $read = <TMPFH>;
    close *TMPFH;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $read, (MESSAGE1 . "\n" . MESSAGE1 . "\n"), 'dual log to file';
}

=head2 Test 10: delete fh sink from channel

delete SINK1 from TESTCHAN1

log MESSAGE2 to TESTCHAN1 at level 0

test no exception thrown

=cut

{
  my $ok = 0;

  eval {
    Log::Info::delete_sink(TESTCHAN1, SINK1);
    Log(TESTCHAN1, 0, MESSAGE2);
    $ok = 1;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'delete fh sink from channel';
}

=head2 Test 11: message not logged to deleted channel

Test nothing to read on $in

=cut

{
  my @ready;
  eval {
    my $s = IO::Select->new;
    $s->add($in);
    @ready = $s->can_read(0);
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
  }

  ok $#ready, -1, 'message not logged to deleted channel';
}

=head2 Test 12: message logged to file

Test last line of F<tmpfile> is MESSAGE2.

=cut

{
  my @read;

  eval {
    open *TMPFH, TMPNAM1;
    local $/ = "\n";
    chomp (@read = <TMPFH>);
    close *TMPFH;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
  }

  ok $read[-1], MESSAGE2, 'message logged to file';
}

=head2 Test 13: delete file sink from channel

test no exception thrown

=cut

{
  my $ok = 0;

  eval {
    Log::Info::delete_sink(TESTCHAN1, SINK2);
    $ok = 1;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'delete file sink from channel';
}

=head2 Test 14: add size-limited file sinks to channel, log series of messages

Truncate TMPNAM{1,2}.

Add sink for each TMPNAM to TESTCHAN, with MAXSIZE.

Log enough messages so that each file should be rotated.

Use translators to test sizes account for translation, too.

=cut

my $messagecount = 1+int(MAXMAXSIZE / (length(MESSAGE1)+length(MESSAGE2)));

{
  my $ok = 0;
  eval {
    truncate $_, 0
      for TMPNAM1, TMPNAM2;

    Log::Info::add_sink    (TESTCHAN1, SINK1, 'FILE', undef,
                            { fn      => TMPNAM1,
                              maxsize => MAXSIZE1, });
    Log::Info::add_sink    (TESTCHAN1, SINK2, 'FILE', undef,
                            { fn      => TMPNAM2,
                              maxsize => MAXSIZE2, });

    Log::Info::add_chan_trans(TESTCHAN1, TRANS1);
    Log::Info::add_sink_trans(TESTCHAN1, SINK1, TRANS2);

    for ((undef) x $messagecount) {
      Log(TESTCHAN1, 0, MESSAGE1);
      Log(TESTCHAN1, 0, MESSAGE2);
    }

    $ok = 1;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'add size-limited file sinks to channel, log series of messages';
}

my @tmpnam1 = map glob("$_*"), TMPNAM1;
@tmpnam1    = @tmpnam1[1..$#tmpnam1,0]; # files in descending age order
my @tmpnam2 = map glob("$_*"), TMPNAM2;
@tmpnam2    = @tmpnam2[1..$#tmpnam2,0]; # files in descending age order

=head2 Test 15: all messages logged to rotated sink1

=cut

{
  my $ok = 0;

  eval {
    my %count;
    local $/ = "\n";
    my $last;
    for my $tmpnam (@tmpnam1) {
      open *TMPFH, $tmpnam;
      while (<TMPFH>) {
        if ( index($_, MESSAGE1) >= 0 ) {
          die "Expected MESSAGE2\n"
            if defined $last and $last eq MESSAGE1;
            $count{MESSAGE1()}++;
            $last = MESSAGE1;
        } elsif ( index($_, MESSAGE2) >= 0 ) {
          die "Expected MESSAGE1\n"
            unless $last eq MESSAGE1;
            $count{MESSAGE2()}++;
            $last = MESSAGE2;
        } else {
          die "Got bad log line: $_\n";
        }
      }
      close *TMPFH;
    }
    $ok = $count{MESSAGE1()} == $messagecount &&
          $count{MESSAGE2()} == $messagecount;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'all messages logged to rotated sink1';
}

=head2 Test 16: all messages logged to rotated sink2

=cut

{
  my $ok = 0;

  eval {
    my %count;
    local $/ = "\n";
    my $last;
    for my $tmpnam (@tmpnam2) {
      open *TMPFH, $tmpnam;
      while (<TMPFH>) {
        if ( index($_, MESSAGE1) >= 0 ) {
          die "Expected MESSAGE2\n"
            if defined $last and $last eq MESSAGE1;
            $count{MESSAGE1()}++;
            $last = MESSAGE1;
        } elsif ( index($_, MESSAGE2) >= 0 ) {
          die "Expected MESSAGE1\n"
            unless $last eq MESSAGE1;
            $count{MESSAGE2()}++;
            $last = MESSAGE2;
        } else {
          die "Got bad log line: $_\n";
        }
      }
      close *TMPFH;
    }
    $ok = $count{MESSAGE1()} == $messagecount &&
          $count{MESSAGE2()} == $messagecount;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'all messages logged to rotated sink2';
}

=head2 Test 17: log rotated at appropriate size (sink1)

Test: each file to be less than or equal to MAXSIZE1 in size, and for every
file other than the last, the first message of the next file should have taken
them over the limit.

=cut

{
  my $ok = 0;

  eval {
    for (my $i = 0; $i < @tmpnam1; $i++) {
      $_ = $tmpnam1[$i];
      my $size = -s $_;
      die sprintf("File %s too large: %d\n", $_, $size)
        if $size > MAXSIZE1;
      if ( $i < $#tmpnam1 ) {
        my $nextfn = $tmpnam1[$i+1];
        local $/ = "\n";
        open *FH, $nextfn;
        my $line = <FH>;
        close *FH;
        die sprintf("Message could have fitted into %s (%d): %s (%d)\n",
                    $_, $size, $line, length($line))
          if ( ($size + length($line)) <= MAXSIZE1 );
      }
    }
    $ok = 1;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'log rotated at appropriate size (sink1)';
}

=head2 Test 18: log rotated at appropriate size (sink2)

As for Test 17, for sink2

=cut

{
  my $ok = 0;

  eval {
    for (my $i = 0; $i < @tmpnam2; $i++) {
      $_ = $tmpnam2[$i];
      my $size = -s $_;
      die sprintf("File %s too large: %d\n", $_, $size)
        if $size > MAXSIZE2;
      if ( $i < $#tmpnam2 ) {
        my $nextfn = $tmpnam2[$i+1];
        local $/ = "\n";
        open *FH, $nextfn;
        my $line = <FH>;
        close *FH;
        die sprintf("Message could have fitted into %s (%d): %s (%d)\n",
                    $_, $size, $line, length($line))
          if ( ($size + length($line)) <= MAXSIZE2 );
      }
    }
    $ok = 1;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'log rotated at appropriate size (sink2)';
}

=head2 Test 19: log switch at whole logged message point (sink1)

for each output file from SINK1, test that the last character is a newline

=cut

{
  my $ok = 0;

  eval {
    for (@tmpnam1) {
      open *TMPFH, $_;
      seek *TMPFH, -1, SEEK_END;
      my $a;
      read *TMPFH, $a, 1;
      close *TMPFH;
      die "Last character of $_ not newline:$a:\n"
        unless $a eq "\n";
    }
    $ok = 1;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'log switch at whole logged message point (sink1)';
}

=head2 Test 20: log switch at whole logged message point (sink2)

for each output file from SINK2, test that the last character is a newline

=cut

{
  my $ok = 0;

  eval {
    for (@tmpnam2) {
      open *TMPFH, $_;
      seek *TMPFH, -1, SEEK_END;
      my $a;
      read *TMPFH, $a, 1;
      close *TMPFH;
      die "Last character of $_ not newline:$a:\n"
        unless $a eq "\n";
    }
    $ok = 1;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'log switch at whole logged message point (sink2)';
}

=head2 Test 21: no messages logged to deleted fh sink

Test nothing to read on $in

=cut

{
  my @ready;
  eval {
    my $s = IO::Select->new;
    $s->add($in);
    @ready = $s->can_read(0);
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
  }

  ok $#ready, -1, 'no messages logged to deleted fh sink';
}
