# (X)Emacs mode: -*- cperl -*-

use strict;

=head1 Unit Test Package for Log::Info functions

This package tests the enable_file_channel function of Log::Info.

=cut

use Fatal      1.02 qw( open seek close );
use Fcntl      1.03 qw( :seek );
use FindBin    1.42 qw( $Bin );
use File::Temp 0.12 qw( tempfile tmpnam );
use Test       1.13 qw( ok plan );

BEGIN { unshift @INC, $Bin };

use test qw( evcheck save_output restore_output );


# Sink names for playing with
use constant SINK1 => 'sink1';

# Message texts for playing with
use constant MESSAGE1 => 'Mickey Murphy';
use constant MESSAGE2 => 'PC McGarry';
use constant MESSAGE3 => 'Captain Snort';
use constant MESSAGE4 => 'Sergeant Major Grout';

BEGIN {
  plan tests  => 10;
       todo   => [],
       ;
}

# -------------------------------------

sub read_file {
  my ($fn) = @_;
  open my $fh, '<', $fn;
  local $/ = undef;
  my $contents = <$fh>;
  close $fh;
  return $contents;
}

sub read_fh {
  my ($fh) = @_;
  my $pos = tell $fh;
  seek $fh, 0, SEEK_SET;
  local $/ = undef;
  my $contents = <$fh>;
  seek $fh, $pos, SEEK_SET;
  return $contents;
}

# ----------------------------------------------------------------------------

use Log::Info qw( :default_channels :log_levels Log );

=head2 Test 1: compilation

This test confirms that the test script and the modules it calls compiled
successfully.

=cut

ok 1, 1, 'compilation';

# -------------------------------------

my $tmpfn = tmpnam;

=head2 Tests 2--4: log to file

Invoke

  enable_file_channel(CHAN_PROGRESS, $tmpfn, '*test A*', 'tmpfile');

(1) Test no exception thrown.

Log MESSAGE1 to CHAN_PROGRESS (at level LOG_INFO)

(2) Test no exception thrown.

(3) Check message is written to file.

=cut

ok evcheck(sub {
             Log::Info::enable_file_channel
               (CHAN_PROGRESS, $tmpfn, '*test A*', 'tmpfile');
           }), 1, 'log to file (1)';

ok evcheck(sub {
             Log(CHAN_PROGRESS, LOG_INFO, MESSAGE1);
           }), 1, 'log to file (2)';

ok read_file($tmpfn), MESSAGE1 . "\n", 'log to file (3)';
unlink $tmpfn
  unless $ENV{TEST_DEBUG};

# -------------------------------------

my $tmpfh = tempfile;

=head2 Tests 5--7: log to file descriptor

Invoke

  enable_file_channel(CHAN_PROGRESS, ':' .fileno($tmpfh), '*test B**', 'tmpfile');

(1) Test no exception thrown.

Log MESSAGE2 to CHAN_PROGRESS (at level LOG_INFO)

(2) Test no exception thrown.

(3) Check message is written to file.

=cut

ok evcheck(sub {
             Log::Info::enable_file_channel
               (CHAN_PROGRESS, ':' . fileno($tmpfh), '*test B**', 'tmpfile');
           }), 1, 'log to file descriptor (1)';

ok evcheck(sub {
             Log(CHAN_PROGRESS, LOG_INFO, MESSAGE2);
           }), 1, 'log to file descriptor (2)';

ok read_fh($tmpfh), MESSAGE2 . "\n", 'log to file descriptor (3)';

# -------------------------------------

=head2 Tests 8--10: log to stderr

Invoke

  enable_file_channel(CHAN_PROGRESS, '', '*test C*', 'tmpfile');

(1) Test no exception thrown.

Log MESSAGE3 to CHAN_PROGRESS (at level LOG_INFO)

(2) Test no exception thrown.

(3) Check message is written to file.

=cut

ok evcheck(sub {
             Log::Info::enable_file_channel
               (CHAN_PROGRESS, '', '*test C*', 'tmpfile');
           }), 1, 'log to stderr (1)';

my $stderr;
ok evcheck(sub {
             save_output('stderr', *STDERR{IO});
             Log(CHAN_PROGRESS, LOG_INFO, MESSAGE3);
             $stderr = restore_output('stderr');
           }), 1, 'log to stderr (2)';

ok $stderr, MESSAGE3 . "\n", 'log to stderr (3)';

# -------------------------------------
