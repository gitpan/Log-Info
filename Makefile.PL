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

use constant NAME         => 'Log-Info';
use constant VERSION_FROM => catfile (qw( lib Log Info.pm ));
use constant AUTHOR       => 'Martyn J. Pearce fluffy@cpan.org';
use constant ABSTRACT     => 'Single interface for log output';

use FindBin 1.42 qw( $Bin );
use lib $Bin;
use make;
