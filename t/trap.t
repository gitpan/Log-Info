# (X)Emacs mode: -*- cperl -*-

use strict;

=head1 Unit Test Package for Log::Info functions

This package tests the use of trapping warn()/die() in Log::Info.

=cut

use Carp            qw( carp croak );
use FindBin    1.42 qw( $Bin );
use Test       1.13 qw( ok plan );

BEGIN { unshift @INC, $Bin };

use test      qw( evcheck save_output restore_output );
use test2     qw( -no-ipc-run runcheck );

# Sink names for playing with
use constant SINK1 => 'sink1';

# Message texts for playing with
use constant MESSAGE1 => 'Mrs. Cobbit';
use constant MESSAGE2 => 'Philby';
use constant MESSAGE3 => 'Chippy Minton';
use constant MESSAGE4 => 'Raggy Dan';

BEGIN {
  plan tests  => 29;
       todo   => [],
       ;
}

# ----------------------------------------------------------------------------

use Log::Info qw( :default_channels Log );

=head2 Test 1: compilation

This test confirms that the test script and the modules it calls compiled
successfully.

=cut

ok 1, 1, 'compilation';

# -------------------------------------

=head2 Test 2: blankety blank

This is a blank test, left here because I cannot be bothered renumbering the
doco.

=cut

ok 1;

# -------------------------------------

=head2 Test 3: adding a sink

This test deletes the C<SINK_STDERR> sink, and adds a sink (C<SINK1), to
C<CHAN_INFO>.  The test is that no exception is thrown.

=cut

my @mess;

ok evcheck(sub {
             Log::Info::delete_sink(CHAN_INFO, SINK_STDERR);
             Log::Info::add_sink (CHAN_INFO, SINK1, 'SUBR', undef,
                                  { subr => sub { push @mess, $_[0] }});
           }, 'adding a sink'), 1, 'adding a sink';

# -------------------------------------

=head2 Test 4: logging some messages

This test writes two log messages to the channel with the sink.  The test is
that no exception is thrown.

This also tests that the explicit import of C<Log> works.  The second message
is logged at level 10, equal to the channel level.  This is expected to get
logged.

=cut

ok evcheck(sub {
             Log (CHAN_INFO, 3, MESSAGE1);
             Log (CHAN_INFO, 5, MESSAGE2);
           }, 'logging a message'), 1, 'logging a message';

# -------------------------------------

=head2 Tests 5--6: checking the messages

These tests check that the expected messages have been passed to the log
subroutine.

=cut

ok shift @mess, MESSAGE1, 'checking the messages (1)';
ok shift @mess, MESSAGE2, 'checking the messages (2)';

# -------------------------------------

=head2 Tests 7--9: checking that warn is not logged

This test invokes warn, and checks (1) that no exception is thrown, (2) that
it is not logged (to CHAN_INFO), and (3) that the message is written to stderr
as usual.

This is to check that nothing is messing with warn by default.

=cut

my $stderr;
ok(evcheck(sub {
             save_output('stderr', *STDERR{IO});
             warn MESSAGE3 . "\n";
             $stderr = restore_output('stderr');
           }, 'checking that warn is not logged (1)'),
   1, 'checking that warn is not logged (1)');

ok scalar(@mess), 0, 'checking that warn is not logged (2)';
ok $stderr, MESSAGE3 . "\n", 'checking that warn is not logged (3)';

# -------------------------------------

=head2 Tests 10--12: checking that die is not trapped

This test invokes C<die> (within an C<eval>), and checks (1) that an exception
is thrown, (2) that it is not logged (to CHAN_INFO), and (3) that the message
is trapped in $@ as usual.

This is to check that nothing is messing with die by default.

=cut

# Can't use evcheck here, as that traps the die!
{
  my $ok = 0;
  eval {
    die MESSAGE4 . "\n";
  }; if ($@) {
    $ok = 1;
  }
  ok $ok, 1, 'checking that die is not trapped (1)';
}

ok scalar(@mess), 0, 'checking that die is not trapped (2)';
ok $@, MESSAGE4 . "\n", 'checking that die is not trapped (3)';

# -------------------------------------

=head2 Test 13: invoking trap_warn_die

Invoke the C<trap_warn_die> function of C<Log::Info>.  Test that no exception
is thrown.

=cut

ok(evcheck(sub {
             Log::Info::trap_warn_die;
           }, 'invoking trap_warn_die'),
   1, 'invoking trap_warn_die');

# -------------------------------------

=head2 Tests 14--17: checking that warn is now logged

This test invokes warn, and checks (1) that no exception is thrown, (2) that
exactly one message has been logged to CHAN_INFO, (3) the the message is
MESSAGE2, and (4) that the message is not written to stderr as usual.

=cut

ok(evcheck(sub {
             save_output('stderr', *STDERR{IO});
             warn MESSAGE2 . "\n";
             $stderr = restore_output('stderr');
           }, 'checking that warn is now logged (1)'),
   1, 'checking that warn is now logged (1)');

ok scalar(@mess), 1, 'checking that warn is now logged (2)';
ok $mess[0], MESSAGE2 . "\n", 'checking that warn is now logged (3)';
ok $stderr, '', 'checking that warn is now logged (4)';
@mess = ();

# -------------------------------------

=head2 Tests 18--21: checking that die messages are now trapped

This test invokes C<die> (within an C<eval>), and checks (1) that an exception
is thrown, (2) that exactly one message has been logged to CHAN_INFO, (3) the
the message is MESSAGE1, and (4) that the message is trapped in C<$@> as
usual.

This is to check that nothing is messing with die by default.

=cut

# Can't use evcheck here, as that traps the die!
{
  my $ok = 0;
  eval {
    die MESSAGE1, "\n";
  }; if ($@) {
    $ok = 1;
  }
  ok $ok, 1, 'checking that die messages are now trapped (1)';
}

ok scalar(@mess), 1, 'checking that die is now trapped (2)';
ok $mess[0], MESSAGE1 . "\n", 'checking that die is now trapped (3)';
ok $@, MESSAGE1 . "\n", 'checking that die is now trapped (4)';
@mess = ();

# -------------------------------------

=head2 Tests 22--25: checking that croak messages are now trapped

This test invokes C<Carp::croak> (within an C<eval>), and checks (1) that an
exception is thrown, (2) that exactly one message has been logged to
CHAN_INFO, (3) the the message begins with MESSAGE1, and (4) that the message
is trapped in C<$@> as usual.

This is to check that nothing is messing with die by default.

=cut

# Can't use evcheck here, as that traps the die!
{
  my $ok = 0;
  eval {
    croak MESSAGE1, "\n";
  }; if ($@) {
    $ok = 1;
  }
  ok $ok, 1, 'checking that die messages are now trapped (1)';
}

ok scalar(@mess), 1, 'checking that die is now trapped (2)';
ok $mess[0], qr/^${\ MESSAGE1() }/, 'checking that die is now trapped (3)';
ok $@, qr/^${\ MESSAGE1() }/, 'checking that die is now trapped (4)';
@mess = ();

# -------------------------------------

=head2 Tests 26--29: checking that carp is now logged

This test invokes C<Carp::carp>, and checks (1) that no exception is thrown,
(2) that exactly one message has been logged to CHAN_INFO, (3) the the message
begins with MESSAGE2, and (4) that the message is not written to stderr as
usual.

=cut

ok(evcheck(sub {
             save_output('stderr', *STDERR{IO});
             carp MESSAGE2 . "\n";
             $stderr = restore_output('stderr');
           }, 'checking that warn is now logged (1)'),
   1, 'checking that warn is now logged (1)');

ok scalar(@mess), 1, 'checking that warn is now logged (2)';
ok $mess[0], qr/^${\ MESSAGE2() }/, 'checking that warn is now logged (3)';
ok $stderr, '', 'checking that warn is now logged (4)';
@mess = ();

# -------------------------------------
