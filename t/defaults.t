# (X)Emacs mode: -*- cperl -*-

use strict;

=head1 Unit Test Package for Log::Info defaults

This package tests the defaults of Log::Info

=cut

use Test                  qw( ok plan );

# Channel names for playing with
use constant TESTCHAN1 => 'testchan1';
use constant TESTCHAN2 => 'testchan2';

BEGIN {
  plan tests  => 1;
       todo   => [],
       ;
}

use Log::Info qw( :DEFAULT :log_levels :default_channels );

=head2 Test 1: compilation

This test confirms that the test script and the modules it calls compiled
successfully.

Log::Info is imported as

  use Log::Info qw( :DEFAULT :log_levels :default_channels );

This tests bug #001

=cut

ok 1, 1, 'compilation';

