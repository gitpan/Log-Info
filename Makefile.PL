use 5.005_62;
use strict;

use File::Spec::Functions qw( catfile );

use constant MOD_REQS =>
  [
   { name    => 'Pod::Usage',
     version => '1.12', },

   { name    => 'Class::MethodMaker',
     package => 'Class-MethodMaker',
     version => '1.02', },

   { name    => 'Term::ProgressBar',
     package => 'Term-ProgressBar',
     version => '2.00',
     optional => 1,
     message => <<"END",
Term::ProgressBar not found.  This is not fatal, but means that you cannot
use the cunning SINK_TERM_PROGRESS sink
END
   },

  ];

use constant EXEC_REQS =>
  [
  ];

use POSIX qw( strftime );
BEGIN {
  my $check = strftime('%z',localtime);
  if ( $check eq '%z' ) {
    print STDERR <<'END';
Beware!  Since your strftime is so backward as to not support %z, I am going
to fake it --- but this means that processes running whilst timezones change
under them will present incorrect timezone offset info.  This is normally an
issue only for processes running when daylight savings comes into force.

If I cannot find a version of gnu date (at runtime), then I cannot fake it up,
either.  Sorry about that.

Currently this only affects the TRANS_CDT translator.


END
  }
}

use constant NAME         => 'Log-Info';
use constant VERSION_FROM => catfile (qw( lib Log Info.pm ));
use constant AUTHOR       => 'Martyn J. Pearce fluffy@cpan.org';
use constant ABSTRACT     => 'Single interface for log output';

use FindBin 1.42 qw( $Bin );
use lib $Bin;
use make;
