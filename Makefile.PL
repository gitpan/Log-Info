require 5.005_62;

use Env                   qw( @PATH );
use ExtUtils::MakeMaker;
use File::Spec::Functions qw( catfile );

#XXX check against pmcompat

use strict;
use constant MOD_REQS =>
  [
   # Required keys: name
   # Optional keys: package (missing => core)
   #                version (missing => not checked) (*string*)
   { name    => 'Pod::Usage',
     version => '1.12', },

#   { name    => 'Class::MethodMaker',
#     package => 'Class-MethodMaker',
#     version => '1.02', },
  ];

use constant EXEC_REQS =>
  # vopt is a switch to pass to the executable to cause it output its version.
  # vexpect is the expected return value after passing vopt to the program.
  # vexpect default is 0.
  [
#   { name    => 'lame',
#     version => '3.70',
#     vopt    => '--help',
#   },

#   { name    => 'mp3id',
#     version => '0.4',
#     vopt    => '--help',
#     vexpect => 255,
#   },
  ];

use constant NAME         => 'Log-Info';
use constant VERSION_FROM => catfile (qw( lib Log Info.pm ));
use constant AUTHOR       => 'Martyn J. Pearce fluffy@cpan.org';
use constant ABSTRACT     => 'Single interface for log output';

# ----------------------------------------------------------------------------
# No user-servicable parts below
# ----------------------------------------------------------------------------

use constant TYPE_EXEC => 'executable';
use constant TYPE_MOD  => 'module';
use constant TYPES     => [ TYPE_EXEC, TYPE_MOD ];

use constant CONFIG =>
  {
   TYPE_MOD  , { defaults => { package => 'core perl',
                             },
                 find     => sub { eval "require $_[0]"; $@ eq '' },
                 vers     => sub {
                   no strict 'refs';
                   # Fool emacs indenter
                   my $_x = q={=; my $pv = ${"$_[0]::VERSION"};
                   return defined $pv ? $pv : -1;
                 },
               },
   TYPE_EXEC , { defaults => { vexpect => 0, },
                 find     => sub {
                   my ($name) = @_;
                   my $exec;
                 PATH_COMPONENT:
                   for my $path (@PATH) {
                     my $try = catfile $path, $name;
                     if ( -x $try ) {
                       $exec = $try;
                       last PATH_COMPONENT;
                     }
                   }
                   defined $exec;
                 },
                 vers     => sub {
                   my ($name, $vopt, $expect) = @_;
                   die "Cannot test version of $name without vopt\n"
                     unless defined $vopt;
                   my $cmd = join ' ', $name, $vopt;
                   my $vstr = qx($cmd 2>&1);
                   my $rv = $? >> 8;
                   die sprintf "Command $cmd exited with value: $rv\n"
                     if $rv != $expect;
                   return $vstr =~ /\b(\d+\.\d+)(?!\d)/ ? $1 : -1;
                 },
               },
  };

# Main -----------------------------------------------------------------------

# Self Test

# Find Module (no version)
check([{ name => 'integer' , type => TYPE_MOD, }])
  and die "Internal Check (1) failed\n";
# Fail module (no version)
check([{ name => 'flubble' , type => TYPE_MOD, }])
  or die "Internal Check (2) failed\n";
# Find module, wrong version
check([{ name => 'IO'      , type => TYPE_MOD, version => '100.0', }])
  or die "Internal Check (3) failed\n";
# Find module, right version
check([{ name => 'IO'      , type => TYPE_MOD, version => '1.00',  }])
  and die "Internal Check (4) failed\n";

# Find exec (no version)
  # Use more (common to dog/windoze too!) (mac?)
check([{ name => 'more'    , type => TYPE_EXEC, }])
  and die "Internal Check (5) failed\n";
# Fail exec (no version)
check([{ name => ' wibwib' , type => TYPE_EXEC, }])
  or die "Internal Check (6) failed\n";
# Find exec, wrong version
  # Could do with one that works on dog/windoze/mac...
check([{ name => 'cut'     , type => TYPE_EXEC,
         version => '100.0', vopt => '--version', }])
  or die "Internal Check (7) failed\n";
# Find exec, right version
check([{ name => 'cut'     , type => TYPE_EXEC,
         version => '1.0', vopt => '--version', }])
  and die "Internal Check (8) failed\n";

# -------------------------------------

my @missing;

$_->{type} = TYPE_MOD
  for @{MOD_REQS()};
$_->{type} = TYPE_EXEC
  for @{EXEC_REQS()};

push @missing, check(MOD_REQS, 1), check(EXEC_REQS, 1)
  # We're at the top level, so caller is defined only if we're being read in
  # from another perl program.  If so, don't perform the checks, to avoid
  # unwanted output.  An example such program is one that reads this file to
  # gather package info.
  unless defined caller;

warn_missing(\@missing);

exit 2
  if grep $_->{type} eq TYPE_MOD, @missing;

WriteMakefile
  (NAME         => NAME,
   VERSION_FROM => VERSION_FROM,
   AUTHOR       => AUTHOR,
   ABSTRACT     => ABSTRACT,
   PREREQ_PM    => { map (($_->{name} => $_->{version} || 0 ), @{MOD_REQS()})},
   EXE_FILES    => [ grep !/(?:CVS|~)$/, glob catfile (qw( bin * )) ],
  );

# End of Main ----------------------------------------------------------------

sub warn_missing {
  my ($missing) = @_;

  my ($type_max) = sort { $b <=> $a } map length $_->{type}, @$missing;
  my ($name_max) = sort { $b <=> $a } map length $_->{name}, @$missing;

  for (@$missing) {
    my ($type, $name, $pkg, $vers, $pv) = @{$_}{qw( type name package
                                                    vers_req vers_fnd )};

    if ( defined $pv ) {
      print STDERR sprintf("%-${type_max}s %${name_max}s requires version " .
                           "$vers (found $pv)",
                           $type, $name)
    } else {
      print STDERR sprintf("Couldn't find %${type_max}s %${name_max}s",
                           $type, $name);
    }

    print STDERR " (from $pkg)"
      if defined $pkg;
    print STDERR "\n";
  }
}

# -------------------------------------

sub check {
  my ($items, $verbose) = @_;

  my ($type_max) = sort { $b <=> $a } map length $_->{type}, @$items;

  foreach my $item (@$items) {
    my $type = $item->{type};
    my $defaults = CONFIG->{$type}->{defaults};
    $item->{$_} = $defaults->{$_}
      for grep ! exists $item->{$_}, keys %$defaults;
    my ($name, $pkg, $vers, $vopt, $vexpect) =
      @{$item}{qw( name package version vopt vexpect)};

    printf STDERR "Checking for %${type_max}s %s...", $type, $name
      if $verbose;
    if ( CONFIG->{$type}->{find}->($name) ) {
      print STDERR " found\n"
        if $verbose;

      if ( defined $vers ) {
        my $vfound = CONFIG->{$type}->{vers}->($name, $vopt, $vexpect);

        return { type     => $type,
                 name     => $name,
                 package  => $pkg,
                 vers_req => $vers,
                 vers_fnd => $vfound,
               }
          if $vers > $vfound;
      }
    } else {
      print STDERR " failed\n"
        if $verbose;
      return { type     => $type,
               name     => $name,
               package  => $pkg,
               vers_req => $vers,
             };
    }
  }

  return;
}

