# (X)Emacs mode: -*- cperl -*-

use strict;

=head1 Unit Test Package for Log::Info functions

This package tests the use of sinks in Log::Info

=cut

use Test                  qw( ok plan );

# Channel names for playing with
use constant TESTCHAN1 => 'testchan1';
use constant TESTCHAN2 => 'testchan2';

# Sink names for playing with
use constant SINK1 => 'sink1';
use constant SINK2 => 'sink2';

# Message texts for playing with
use constant MESSAGE1 => 'Windy Miller';
use constant MESSAGE2 => 'Mrs. Murphy';

BEGIN {
  plan tests  => 15;
       todo   => [],
       ;
}

use Log::Info qw( Log );

=head2 Test 1: compilation

This test confirms that the test script and the modules it calls compiled
successfully.

=cut

ok 1, 1, 'compilation';

=head2 Test 2: adding a channel

This test adds a channel.  The test is that this occurs without error.

=cut

{
  my $ok = 1;
  eval {
    Log::Info::add_channel (TESTCHAN1);
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'adding a channel';
}

=head2 Test 3: adding a sink

This test adds a sink.  The test is that no exception is thrown.

=cut

my @mess;

{
  my $ok = 1;
  eval {
    Log::Info::add_sink (TESTCHAN1, SINK1, 'SUBR', undef,
	                 { subr => sub { push @mess, $_[0] }});
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'adding a sink';
}

=head2 Test 4: logging some messages

This test writes two log messages to the channel with the sink.  The test is
that no exception is thrown.

This also tests that the explicit import of C<Log> works.  The second message
is logged at level 10, equal to the channel level.  This is expected to get
logged.

=cut

{
  my $ok = 1;
  eval {
    Log (TESTCHAN1, 3, MESSAGE1);
    Log (TESTCHAN1, 5, MESSAGE2);
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'logging a message';
}

=head2 Tests 5--6: checking the messages

These tests check that the expected messages have been passed to the log
subroutine.

=cut

ok shift @mess, MESSAGE1, 'checking the messages (1)';
ok shift @mess, MESSAGE2, 'checking the messages (2)';

=head2 Test 7: resetting the channel level

This test sets the channel level to 8.  The test is that no exception is
thrown.

=cut

{
  my $ok = 1;
  eval {
     Log::Info::set_channel_out_level (TESTCHAN1, 8);
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'resetting the channel level';
}

=head2 Test 8: logging above level

This test writes a log messages to channel with the sink at log level 10.  The
test is that no exception is thrown, but no message is logged.

This also tests that the previous setting to level 8 worked.

=cut

{
  my $ok = 0;
  eval {
    Log (TESTCHAN1, 10, MESSAGE1);
    $ok = ! @mess;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'logging above level';
  @mess = ();
}

=head2 Test 9: resetting the sink level

This test increases the sink output level to 3.  The test is that no exception
is thrown.

=cut

{
  my $ok = 1;
  eval {
    Log::Info::set_sink_out_level (TESTCHAN1, SINK1, 3);
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'resetting the sink level';
}

=head2 Test 10: logging between levels

This test writes a log messages to channel with the sink at log level 5.  The
test is that no exception is thrown, but no message is logged.

This also tests that the previous setting of the sink level to 8 worked.

=cut

{
  my $ok = 0;
  eval {
    Log (TESTCHAN1, 5, MESSAGE1);
    $ok = ! @mess;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'logging between levels';
  @mess = ();
}

=head2 Test 11: logging below all levels

This test writes a log messages to channel with the sink at log level 2.  The
test is that no exception is thrown, and the message is logged.

=cut

{
  my $ok = 0;
  eval {
    Log (TESTCHAN1, 2, MESSAGE1);
    $ok = ( @mess == 1 ) && ( $mess[0] eq MESSAGE1 );
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'logging below all levels';
  @mess = ();
}

=head2 Test 12: resetting the channel level to undef

This test sets the channel output level to undef.  The test is that no
exception is thrown.

=cut

{
  my $ok = 0;
  eval {
    Log::Info::set_channel_out_level(TESTCHAN1, undef);
    $ok = 1;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'resetting the channel level to undef';
}

=head2 Test 13: resetting the sink level to undef

This test sets the sink output level to undef.  The test is that no exception
is thrown.

=cut
{
  my $ok = 0;
  eval {
    Log::Info::set_sink_out_level(TESTCHAN1, SINK1, undef);
    $ok = 1;
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'resetting the sink level to undef';
}

=head2 Test 14: logging a message with channel, sink levels set to undef

This test writes a log messages to channel with the sink at log level 50.  The
test is that no exception is thrown, and the message is logged.

=cut

{
  my $ok = 0;
  eval {
    Log (TESTCHAN1, 50, MESSAGE2);
    $ok = ( @mess == 1 ) && ( $mess[0] eq MESSAGE2 );
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'logging a message with channel, sink levels set to undef';
  @mess = ();
}

=head2 Test 15: logging a message at channel -> undef, sink -> 10

This test sets the sink output level to 10.  A message is logged at level 5,
then level 15.  The test is that the first message was logged, the second not,
and no exception thrown.

=cut

{
  my $ok = 0;
  eval {
    Log::Info::set_sink_out_level(TESTCHAN1, SINK1, 10);
    Log (TESTCHAN1, 5, MESSAGE1);
    Log (TESTCHAN1, 15, MESSAGE2);
    $ok = ( @mess == 1 ) && ( $mess[0] eq MESSAGE1 );
  }; if ( $@ ) {
    print STDERR "Test failed:\n$@\n"
      if $ENV{TEST_DEBUG};
    $ok = 0;
  }

  ok $ok, 1, 'logging a message at channel -> undef, sink -> 10';
  @mess = ();
}
