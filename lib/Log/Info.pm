# (X)Emacs mode: -*- cperl -*-

#Implement log-input

#XXX Remove dependency to hairy Sys::Syslog

package Log::Info;

=head1 NAME

Log::Info - Single interface for log output

=head1 SYNOPSIS

  use Log::Info qw( :DEFAULT :log_levels :default_channels );

  Log  (CHAN_INFO, LOG_ERR,  "A fatal error occurred");
  Logf (CHAN_INFO, LOG_INFO, "Loading file: %s", $filename);

  Log::Info::add_sink               (CHAN_STATS, 'stats-file', 'FILE',
                                     LOG_INFO,
                                     { fn => "$ENV{HOME}/stats",
                                       maxsize => 10 * 1024**2, # 1M,
                                     });

  Log::Info::add_sink               (CHAN_DEBUG, 'stderr', 'FH', LOG_INFO,
                                     { fh => *STDERR{IO} })
    if $opt_debug;

  Log::Info::set_sink_out_level     (CHAN_INFO, SINK_STDERR, LOG_INFO);

  Log::Info::add_channel            ('MYLOG', $fh);
  Log::Info::set_channel_out_level  ('MYLOG', LOG_WARNING);
  Log::Info::add_sink               ('MYLOG', 'mysink', 'FILE', LOG_ERR,
                                     { fn => '/tmp/mylog' });
  Log                               ('MYLOG', LOG_INFO, 'I got to here...');
  Log::Info::delete_sink            ('MYLOG', 'outf');
  Log::Info::delete_channel         ('MYLOG');

=head1 DESCRIPTION

Log::Info is intended to be a single interface for all logging action.  Each
instance of Log::Info is intended to be an output for a particular type of
log; some defaults are provided, and custom ones may be generated.

Log::Info exports functions C<Log> and C<Logf> by default.

=head2 Concepts

Log::Info is a package, not a class.  There is only one logging mechanism in a
running program; this is considered to be a good thing.  Log::Info knows of
I<channels>, which have I<sinks>; channels are named log facilities, whilst
sinks are the output points.

The idea is that modules each log their messages to some (set of) channels,
each channel representing some type of message (general information,
statistics, progress, etc.), and the controlling script just sets the output
levels and directions of the sinks according to configuration.  Thus, the
communication between the script and the modules is somewhat simplified.

Under these circumstances, the module need only call
L<Log|"Log">/L<Logf|"Logf"> directly, and whether it is used as part of a
daemon process logging to syslog, or a standalone unit dumping errors to
stderr, the choices are made purely by the calling script.

The only thing left to decide is policy, regarding what messages are sent to
which channel, and at what level.  The module enforces no policy, but does
attempt to provide a start point in a set of default channels, and a little
suggested guidance on the use of levels within those channels.  This is
intended to be helpful; any suggestions to enhance these to be more so are
welcomed by the author.

For those wishing to use a different set of policies for whatever reason,
channel creation, etc. are all completely available to the user.

=head2 EXPORTS

All items are exported on request, except where noted

=over 4

=item :trap

Not really an export, but a pragma.  Add handlers to warn(), die(), to log
messages to the log system.

The die handler logs the message to C<CHAN_INFO> at C<LOG_ERR>.  The die
message is still propogated up the call stack, so will typically appear on
stderr.  If CHAN_INFO is directed to stderr, then the error message will
appear twice.

The warn handler logs the message to C<CHAN_INFO> at C<LOG_WARNING>.

This also traps C<Carp> messages, I<as long as this is installed after Carp>
--- so do the C<use Carp> before the C<use Log::Info qw( :trap )>;

=item Log

B<Exported by default>

=item Logf

B<Exported by default>

=item :log_levels

=item :syslog_facilities

=item :default_channels

=back

=cut

# ----------------------------------------------------------------------------

# Pragmas -----------------------------

require 5.005_62;
use strict;
use warnings;

# Inheritance -------------------------

use base qw( Exporter );
our (@EXPORT, @EXPORT_OK, %EXPORT_TAGS);

BEGIN {
  @EXPORT_OK = qw( $PACKAGE $VERSION );
}

# Utility -----------------------------

use Carp                        qw( carp croak );
use Env                         qw( @PATH );
use Fatal                  1.02 qw( :void close open seek sysopen );
use Fcntl                  1.03 qw( O_WRONLY O_APPEND O_CREAT O_EXCL );
use File::Spec::Functions   1.1 qw( catfile );
use FindBin                1.42 qw( $Script );
use IO::Handle             1.21 qw( );
use IO::Pipe              1.121 qw( );
use IO::Select             1.14 qw( );
use POSIX                  1.03 qw( strftime );
use Sys::Syslog            0.01 qw( openlog closelog syslog setlogmask setlogsock );

# fails under 5.6.
# require 'syslog.ph';

# ----------------------------------------------------------------------------

# -------------------------------------
# PACKAGE VARS
# -------------------------------------

my %channel;
our $dying;    # Set to one when calling within a caught 'die'
our $carping;  # Set to one when calling within a caught Carpism

# -------------------------------------
# PACKAGE CONSTANTS
# -------------------------------------

=head1 PACKAGE CONSTANTS

Z<>

=cut

use constant ETA_ACCURACY => 5; # Update progress bar at least this often (in
                               # seconds) to ensure ETA is up-to-date

=head2 Log Levels

The following constants are available for use as arguments to the C<level>
attribute of the L<Log|"Log"> or L<Logf|"Logf"> call (listed in descending
order).  The constants are stolen shamelessly from L<syslog>, and all are
guranteed to be valid levels for a C<SYSLOG>-type sink.  All of these
constants will be imported inidividually on request, or grouped together with
the C<:log_levels> tag.

=over 4

=item LOG_EMERG

system is unusable

=item LOG_ALERT

action must be taken immediately

=item LOG_CRIT

critical conditions

=item LOG_ERR

error conditions

=item LOG_WARNING

warning conditions

=item LOG_NOTICE

normal, but significant, condition

=item LOG_INFO

informational message

=item LOG_DEBUG

debug-level message

=back

=cut

# All valid syslog levels (this is required by SYSLOG type documentation).
use constant LOG_LEVELS       => qw( LOG_EMERG   LOG_ALERT  LOG_CRIT LOG_ERR
                                     LOG_WARNING LOG_NOTICE LOG_INFO
                                     LOG_DEBUG );

=head2 Log facilities

The following constants are available for use as arguments to the C<facility>
attribute of the C<SYSLOG> sink type.  All of these constants will be imported
inidividually on request, or grouped together with the C<:syslog_facilities>
tag.

=over 4

=item FTY_AUTHPRIV

=item FTY_CRON

=item FTY_DAEMON

=item FTY_LPR

=item FTY_MAIL

=item FTY_NEWS

=item FTY_SYSLOG

=item FTY_USER

=item FTY_UUCP

=item FTY_LOCAL0

=item FTY_LOCAL1

=item FTY_LOCAL2

=item FTY_LOCAL3

=item FTY_LOCAL4

=item FTY_LOCAL5

=item FTY_LOCAL6

=item FTY_LOCAL7

=back

=cut

use constant LOG_FACILITIES   => qw( FTY_AUTHPRIV FTY_CRON
                                     FTY_DAEMON FTY_LPR FTY_MAIL FTY_NEWS
                                     FTY_SYSLOG FTY_USER FTY_UUCP
                                     FTY_LOCAL0 FTY_LOCAL1 FTY_LOCAL2
                                     FTY_LOCAL3 FTY_LOCAL4 FTY_LOCAL5
                                     FTY_LOCAL6 FTY_LOCAL7
                                   );

use constant LOG_LEVEL        => { map { $_ => Sys::Syslog::xlate($_) }
                                       LOG_LEVELS
                                 };
use constant LOG_NAME         => { reverse %{LOG_LEVEL()} };

# In ascending numeric order
use constant LOG_LEVEL_VALUES => sort { $a <=> $b } values %{LOG_LEVEL()};

BEGIN {
  # Create constant subs for each log level (to export).
  for (LOG_LEVELS) {
    no strict 'refs';
    *{join('::', __PACKAGE__, $_)} = eval "sub() { LOG_LEVEL->{$_} }";
  }

  # Create constant subs for each log facility (to export).
  for (LOG_FACILITIES) {
    no strict 'refs';
    my $name = lc substr($_, 4);
    *{join('::', __PACKAGE__, $_)} = sub { $name };
  }

  push @EXPORT,    qw( Log Logf );
  push @EXPORT_OK, LOG_LEVELS;
  push @EXPORT_OK, LOG_FACILITIES;
  $EXPORT_TAGS{log_levels}        = [ LOG_LEVELS ];
  $EXPORT_TAGS{syslog_facilities} = [ LOG_FACILITIES ];
}

sub __dump_levels{
  my ($max) = sort { $b <=> $a } map length, LOG_LEVELS;
  printf "%${max}s  % 02d\n", $_, LOG_LEVEL->{$_}
    for LOG_LEVELS;
}

# -------------------------------------

=head2 Default Channel (and Sink) Names

Each of the following channels exist by default, and have their channel level
set to C<undef>.  Only L<CHAN_INFO|"CHAN_INFO"> has a sink by default; called
SINK_STDERR (a name exported with the C<:default_channels> tag), which is a
filehandle to STDERR, and is set at level L<LOG_WARNING|"LOG_WARNING">.

Each channel and sink name will be exported upon request, or together using
the C<:default_channels> tag.

=over 4

=item CHAN_PROGRESS

Intended for progress reports, e.g., C<done 1 of 3 files>, or C<20% through>.

Default level: LOG_WARNING

=item CHAN_DEBUG

Intended for debugging messages, such as those you might output with
C<--debug> flag on.

Default level: LOG_WARNING

=item CHAN_STATS

Intended for output of statistical information; e.g., C<found 300 items> or
C<output file is 30M, parsing took 79s>.

Default level: LOG_WARNING

=item CHAN_INFO

Intended for warning and error messages, and those that would be output by
C<-v>.

Messages that would be used with C<warn> should be logged at level
L<LOG_WARNING|"LOG_WARNING">, those for a C<-v> flag with level
L<LOG_INFO|"LOG_INFO"> (and C<LOG_DEBUG|"LOG_DEBUG"> for increased verbosity).

C<die> messages should be logged at C<LOG_ERR|"LOG_ERR"> level.
L<LOG_EMERG|"LOG_EMERG"> should be reserved for conditions detected which have
a significant, time-critical effect on the operating system as a whole (e.g.,
anything which will cause the operating system to hang or crash).

L<LOG_ALERT|"LOG_ALERT"> should be used for conditions which may affect the
correct operation of the operating system, but will not cause the system to
fail (e.g., detected filesystem faults).

L<LOG_CRIT|"LOG_CRIT"> should be used to indicate that some problem has been
identified that is likely to adversely affect the correct operation of a
system (other than the operating system) of which this program is a part, not
including that this program is going to fail.  An example of this is an error
in a shared configuration file.

L<LOG_NOTICE|"LOG_NOTICE"> should be used for abnormal, but not worrying
conditions.  For example, if a grep-like program might log a message for each
file read at level L<LOG_INFO|"LOG_INFO">, but log at
L<LOG_NOTICE|"LOG_NOTICE"> files which it has not permissions to read.

=back

=cut

use constant SINK_STDERR      => ':stderr';

use constant DEFAULT_CHANNELS => qw( CHAN_PROGRESS CHAN_DEBUG
                                     CHAN_STATS    CHAN_INFO );

BEGIN {
  for (DEFAULT_CHANNELS) {
    no strict 'refs';
    # Prefix with ':' to make illegal name (for anyone else!)
    # (to avoid namespace clash)
    my $name = ':' . lc substr($_, 5);
    *{join('::', __PACKAGE__, $_)} = sub { $name };
  }

  push @EXPORT_OK, DEFAULT_CHANNELS, 'SINK_STDERR';
  $EXPORT_TAGS{default_channels}        = [ DEFAULT_CHANNELS, 'SINK_STDERR' ];
}

# -------------------------------------

=head2 SINK_TERM_PROGRESS

Here is a fine kettle of fish.

This sink sniffs its filehandle (upon sink creation), and if it smells like a
TTY, it uses it as a progress bar.  Otherwise, it just sets up a
file/filehandle sink as usual.

In progress-bar mode, incoming messages are examined.  If they look like

  m!\[([\d_,.]+/[\d_,.]+|[\d_,.]+%)(\s+[^]]*)?\s+Done\]!

Then that is treated as progress information, and the bar updated
accordingly.

=over 4

=item ARGUMENTS

=over 4

=item fh

Filehandle to output to, or name of file.  B<Beware>: if you present a
filehandle, you probably want to provide a glob ref (e.g., C<\*STDERR>); the
C<*foo{THING}> will never act as a terminal.  undef defaults to STDERR.

=item level

B<Optional>  A sink message cutoff level.  Defaults to C<undef>

=cut

# FOR TESTING
our ($__SINK_TERM_FORCE) = 0;

my ($last_time, $last_now);
sub SINK_TERM_PROGRESS {
  eval "use Term::ProgressBar 2.00;";
  croak sprintf("Cannot use sink %s without Term::ProgressBar present:\n  %s",
                (caller 0)[3], $@)
    if $@;

  my ($fh, $level) = @_;

  my $fn;
  if ( defined $fh ) {
    if ( ! ref $fh ) {
      $fn = $fh;
      CORE::sysopen $fh, $fn, O_WRONLY
          or croak "Cannot open $fh for writing: $!\n";
    }
  } else {
    $fh = \*STDERR;
  }

  Term::ProgressBar->__force_term($__SINK_TERM_FORCE)
      if $__SINK_TERM_FORCE;
  if ( $__SINK_TERM_FORCE || -t $fh ) {
    my ($next) = (0);
    my $progress = Term::ProgressBar->new({count => 100,
                                           fh    => $fh,
                                           ETA   => 'linear'});
    return 'SUBR', undef,
           { subr => sub {
               if ( my ($prefix, $now, $end, $percent, $suffix) =
                    ($_[0] =~ m!(.*)
                                \[(?:([\d_,.]+)/([\d_,.]+)  |
                                     (?:([\d_,.]+)%))
                                  (?:\s+[^]]*)?\s+Done\]
                                (.*)!x) ) {
                 tr/_,//d
                   for grep defined, $now, $end, $percent;

                 ($now, $end) = ($percent, 100)
                   if defined $percent;

                 my $message_printed = 0;

                 if ( defined $suffix and $suffix !~ /^\s*$/ ) {
                   s!^\s*(.*?)\s*$!$1!
                     for grep defined, $suffix, $prefix;
                   if ( defined $prefix and $prefix !~ /^\s*$/ ) {
                     $progress->message("$prefix $suffix");
                   } else {
                     $progress->message($suffix);
                   }

                   $message_printed = 1;
                 }

                 if ( $end != $progress->target ) {
                   $progress->target($end);
                   $next = $progress->update($now)
                 } else {
                   $next = $progress->update($now)
                     if($message_printed                  or
                        $now >= $next                     or
                        time >= $last_time + ETA_ACCURACY ) ;
                 }

                 $last_now = $now;
               } else {
                 $progress->message($_[0]);
                 $progress->update($last_now);
               }
             $last_time = time;
           }
         };
  } else {
    if ( defined $fn ) {
      return 'FILE', $level, { fn => $fh };
    } else {
      return 'FH',   $level, { fh => $fh };
    }
  }
}

# -------------------------------------

=head2 Default Translators

Default translator units provided for communal edification.

=over 4

=item TRANS_UDT

(UDT =E<gt> "Un*x-Date-Time").  Prefix each message with the date and time, first
in Un*x (seconds since Jan 1, 1970) format, then as the scalar gmtime output.
gmtime is deliberately chosen to avoid weirdness over, say, daylight-savings
time changes.

=back

=cut

use constant TRANS_UDT =>
  sub { my $time = time;
        sprintf('[%d %s] %s',
                $time, scalar gmtime $time, $_[0]) };

{ # Very unpleasant hackery to discern timezone offset on systems with backward
  # strftimes.  Bloody Solaris.

  my $save = $!+0;

  my $format = '(%d%b %H:%M:%S%z)';
  my $check = strftime('%z',localtime);
  if ( $check eq '%z' ) {
    $format = undef;

  ATTEMPT:
    # sfw for recent Solaris boxen
    for my $path (@PATH, '/opt/sfw/bin') {
      for my $dname (qw( date gdate )) {
        my $date = catfile $path, $dname;
        next
          unless -x $date;

#         pipe my ($read, $write);
#         die "fork failed: $!\n"
#           unless defined (my $pid = fork);
#
#         if ( $pid ) { # Parent
#           close $write;
#           print STDERR $_
#             for <$read>;
#         } else {      # Child
#           close $read;
#           open STDOUT, '>&=', fileno $write;
#           open STDERR, '>&=', fileno $write;
#
#           exec $date, '--version';
#         }

        my $date_version = qx( $date --version 2>&1 );
        {
          local $/ = undef;
          open *DATE, "$date --version 2>&1 |";
          $date_version = <DATE>;
          # Don't check the return code; it'll often be >0 since we're
          # running a utility (--version)
          CORE::close *DATE;
        }

        if ( $date_version =~ m/^date \(GNU.*\) ([\d.]+)$/m ) {
          (my $version = $1);
          my @v = split /\./, $version;
          $version = join('.', $v[0],
                          join '', map sprintf('%03d', $_), @v[1..$#_]);
          if ( $version >= 2 ) {
            chomp(my $timezone = qx( $date +%z ));
            $format   = "(%d%b %H:%M:%S$timezone)";
            last ATTEMPT;
          } # end if ( $version >= 2 )
        } # end if ( $date_version =~ m/^date \(GNU.*\) ([\d.]+)$/m )
      } # for my $dname (qw( date gdate ))
    } # end for my $path (@PATH)

    # A questionable llseek on Solaris leaves ESPIPE in $!
    $! = $save;
  } # end if ( $check eq '%z' )

  use constant TRANS_CDT =>
    sub { my $time = time;
          die "Cannot determine timezone info.  Sorry.  Perhaps installing gnu date will help\n"
            unless defined $format;
          sprintf('[%d%s:%s] %s',
                  $time,
                  strftime($format, localtime($time)),
                  $0, $_[0]); };
}

# -------------------------------------

our $PACKAGE = 'Log-Info';
our $VERSION = '1.20';

# -------------------------------------
# PACKAGE CONSTRUCTION
# -------------------------------------

# -------------------------------------
# PACKAGE DESTRUCTION
# -------------------------------------

END {
  delete_channel($_)
    for keys %channel;
}

# -------------------------------------
# PACKAGE COMPONENTS
# -------------------------------------

=head1 PACKAGE COMPONENTS

Z<>

=cut

# Channels -------------------------------------------------------------------

=head2 CHANNELS

=cut

# Map from channel name to details.
# Each detail is a hashref, with the following keys:
#   sinks   ) Hashref of data sinks, by name.  The name itself is for
#             identifying the sink for adding, removing, altering.  It has no
#             semantic value.
#             Each sink is itself a hashref, with keys:
#               type    )
#                  Currently recognized types are
#                    FILE    )
#                      Values recognized:
#                        fn      ) (base) filename
#                        maxsize ) max file size
#                        fh      ) open fh, if previously used.  This is
#                                  generated and used by Log() directly; do
#                                  not manhandle.
#                    FH      )
#                        fh      ) open fh.  May be an IO thing (*FOO{IO}),
#                                  a glob ref, a glob, or an instance of
#                                  IO::Handle
#                    SUBR    )
#                        subr    ) a subroutine that will be invoked with the
#                                  log text as its single argument.
#               values  )
#                  Hashref with Type-specific keys; see the (type)
#                  documentation
#               trans   )
#                  If defined, a translation applied for the sink.  This is
#                  aplied to the result of any channel-specific translation.
#               level   )
#                  If defined, a level cutoff for the sink.  This level is
#                  checked only if the channel level is passed; hence, a level
#                  greater than or equal to the channel level has no effect.
#   trans   ) Arrayref of sub refs for channel data translators.  Each array
#             member is applied in order, list head first, with cumulative
#             results.
#   level   ) Number for channel level output cutoff

=head2 add_channel

Create a new channel.

=over 4

=item PRECONDITIONS

  chan is not already a channel name

  $chan =~ /^[\w-]+$/;

=item ARGUMENTS

=over 4

=item chan

name of channel

=item level

Optional.  Logging level; defaults to LOG_NOTICE.  Pass C<undef> to log all
messages.

=back

=back

=cut

sub add_channel {
  my ($chan, $level) = @_;

  $level = LOG_NOTICE()
    unless @_ >= 2;

  croak "Invalid channel name :->$chan<-\n"
    unless $chan =~ /^[\w-]+$/ or caller eq __PACKAGE__;

  croak "Channel already exists: $chan\n"
    if exists $channel{$chan};

  $channel{$chan} = { sinks => {},
                      level => $level,
                    };
}

BEGIN {
  add_channel(eval "$_", undef)
    for DEFAULT_CHANNELS;
}

# -------------------------------------

=head2 delete_channel

delete an existing channel.  Implicitly deletes all attached sinks.

=over 4

=item PRECONDITIONS

  chan is an existing channel name

=item ARGUMENTS

=over 4

=item chan

name of channel to delete

=back

=back

=cut

sub delete_channel {
  my ($chan) = @_;

  croak "Channel does not exist: $chan\n"
    unless exists $channel{$chan};

  delete_sink($chan, $_)
    for keys %{$channel{$chan}{sinks}};
  delete $channel{$chan};
}

# -------------------------------------

=head2 channel_exists

=over 4

=item ARGUMENTS

=over 4

=item chan

Channel name to test for

=back

=item RETURNS

=over 4

=item exists

Whether the name channel is known to Log::Info

=back

=back

=cut

sub channel_exists { return exists $channel{$_[0]} }

# -------------------------------------

=head2 set_channel_out_level

set output cutoff level on channel

=over 4

=item ARGUMENTS

=over 4

=item chan

channel to set output cutoff level on

=item lvl

level to set to; subsequent log entries will only be written if they have
level E<lt>= lvl.

=back

=back

=cut

sub set_channel_out_level {
  my ($chan, $level) = @_;

  croak "Channel does not exist: $chan\n"
    unless exists $channel{$chan};
  $level = get_level($level);

  $channel{$chan}->{level} = $level;
}

# -------------------------------------

=head2 add_chan_trans

Add a translator to a channel.

=over 4

=item ARGUMENTS

=over 4

=item chan

The channel to add the translator to.

=item trans

The translator to add.  The translator will be called in order after any
previously added translators, and will be given the results of the log string
having been through those translators.  The results of the translation
provided by this translator will be passed to any translators installed after
this one, and to any sink-specific translators.

=back

=back

=cut

sub add_chan_trans {
  my ($chan, $trans) = @_;

  croak "Channel does not exist: $chan\n"
    unless exists $channel{$chan};
  croak sprintf("Translator for channel %s not a subroutine: %s\n",
                $chan, ref $trans || $trans)
    unless UNIVERSAL::isa ($trans, 'CODE');

  push @{$channel{$chan}{trans}}, $trans;
}

# Sinks ----------------------------------------------------------------------

=head2 SINKS

=cut

# -------------------------------------

=head2 add_sink

=over 4

=item PRECONDITIONS

  $chan is an existing channel name

  $sink =~ /^[\w-]+$/;

=item ARGUMENTS

=over 4

=item chan

channel to add sink to

=item name

name of sink

=item type

sink type as string.  See L<params|"params"> for acceptable types.

=item level

Output cutoff level.  Set to 'undef' to accept any messages accepted by the
channel.  This level is checked after the channel level; therefore, if this
level is higher than the channel level, it will have no effect.

=item params

A hashref of type-specific parameters.  Recognized keys are type specific:

=over 4

=item FILE

Output to file.  If the file exists, it will be appended to.  Each message
(call to L<Log|"Log">) will be newline-terminated.  Keys are:

=over 4

=item fn

Filename

=item maxsize

Optional; maximum filesize.  Files will be closed, datestamped (name will have
date appended) and a new file opened if this size is about to be exceeded.
Defaults to 1Gb.

=back

=item FH

Output to filehandle.  Creation of, and closing of, the filehandle are the
responsibility of the client.  Do not delete the filehandle without closing
the sink first. Each message (call to L<Log|"Log">) will be
newline-terminated.  Keys are:

=over 4

=item fh

Filehandle to output to.  May be an IO handle (*foo{IO}), a glob ref, a glob,
or an instance of IO::Handle.

=back

=item SUBR

Callback subroutine.  Keys are:

=over 4

=item subr

Subr to call back to (once for each call to L<Log|"Log">).  String will be
passed to subr.  No line terminator will be added.

=back

=item SYSLOG

Log to C<syslog> service.  Any C<LOG_I<X>> value provided by this module is a
valid syslog level; any level that is provided that is not valid for syslog is
rounded down to the nearest value.  Any level that is less than all valid
values is defaulted to LOG_EMERG.  The message is logged with the basename of
the running script, and pid.

Due to an artifact of L<Sys::Syslog>, messages have a space appended when they
appear in the log.

Keys are:

=over 4

=item facility

Optional; facility to pass to syslog to log messages under.  Valid values are
the C<FTY_> constants.

=back

=back

=back

=back

=cut

my $syslog_initialized = 0;

use constant REQUIRED_PARAMS =>
  {
   FILE   => [ qw( fn ) ],
   FH     => [ qw( fh ) ],
   SUBR   => [ qw( subr )],
   SYSLOG => [ ],
  };

sub add_sink {
  my ($chan, $name, $type, $level, $params) = @_;

  croak "Channel does not exist: $chan\n"
    unless exists $channel{$chan};
  croak sprintf("params arg must be hashref, not %s\n", ref $params)
    if defined $params and not UNIVERSAL::isa($params, 'HASH');

  croak "Invalid sink name :->$name<-\n"
    unless $name =~ /^[\w-]+$/ or caller eq __PACKAGE__;

  my %values;

  my $required_params = REQUIRED_PARAMS->{$type};
  croak "Unrecognized sink type: $type\n"
    unless defined $required_params;
  croak sprintf ("%s undefined for %s sink type; channel/sink %s/%s\n",
                 $_, $type, $chan, $name)
    for grep ! defined $params->{$_}, @$required_params;

  if ( $type eq 'FILE' ) {
    @values{qw( fn maxsize )} = @{$params}{qw( fn maxsize )};
    $values{maxsize} = 1_024 ** 3 # 1Gb
      unless defined $values{maxsize};
    croak
      sprintf ("maxsize must be greater than 0 for channel/sink %s/%s: %s\n",
               $chan, $name, $values{maxsize})
        unless $values{maxsize} > 0;
  } elsif ( $type eq 'FH' ) {
    $values{fh}               = $params->{fh};
    croak
      sprintf ("fh type not acceptable for channel/sink %s/%s: %s\n",
               $chan, $name, ref $values{fh})
        unless UNIVERSAL::isa ($values{fh}, 'IO::Handle')
            or UNIVERSAL::isa ($values{fh}, 'GLOB');
  } elsif ( $type eq 'SUBR' ) {
    $values{subr}             = $params->{subr};
    croak
      sprintf ("subr type not acceptable for channel/sink %s/%s: %s\n",
               $chan, $name, ref $values{subr})
        unless UNIVERSAL::isa ($values{subr}, 'CODE');
  } elsif ( $type eq 'SYSLOG' ) {
    $values{facility} = $params->{facility};

    {
      no strict 'refs';
      croak
        sprintf ("facility '%s' unrecognized for channel/sink %s/%s\n",
                 $values{facility}, $chan, $name)
          if defined $values{facility} and
            ! grep &$_ eq $values{facility}, LOG_FACILITIES;
    }

    unless ( $syslog_initialized ) {
      setlogsock 'unix';
      openlog $Script, 'cons,pid', 'user';
      $syslog_initialized = 1;
    }
  } else {
    croak "Unrecognized sink type: $type\n";
  }

  my $sink = { type   => $type,
               level  => $level,
               values => \%values,
             };

  $channel{$chan}{sinks}{$name} = $sink;
}

BEGIN {
  add_sink(CHAN_INFO, SINK_STDERR, 'FH', LOG_WARNING, { fh => *STDERR{IO} });
}

# -------------------------------------

=head2 delete_sink

Remove a sink from a channel.

=over 4

=item ARGUMENTS

=over 4

=item chan

Name of the channel to delete the sink from.

=item sink

Name of the sink to delete.

=back

=back

=cut

sub delete_sink {
  my ($chan, $sink) = @_;

  croak "Channel does not exist: $chan\n"
    unless exists $channel{$chan};
  croak "Channel/Sink does not exist: $chan/$sink\n"
    unless exists $channel{$chan}{sinks}{$sink};

  if ( $channel{$chan}{sinks}{$sink}{type} eq 'FILE' ) {
    if ( defined $channel{$chan}{sinks}{$sink}{values}{fh} ) {
      $channel{$chan}{sinks}{$sink}{values}{fh}->close
      or warn sprintf("Log::Info::delete_sink : " .
                      "Close failed on filehandle for channel/sink/filename " .
                      "%s/%s/%s: $!",
                      $chan, $sink, $channel{$chan}{sinks}{$sink}{values}{fn});
    }
  }

  delete $channel{$chan}{sinks}{$sink};
}

# -------------------------------------

=head2 set_sink_out_level

set output cutoff level on channel

=over 4

=item ARGUMENTS

=over 4

=item chan

channel whose sink to amend

=item sink

sink to set output level of

=item lvl

level to set to; subsequent log entries will only be written if they have
level E<lt>= lvl.

=back

=back

=cut

sub set_sink_out_level {
  my ($chan, $sink, $level) = @_;

  croak "Channel does not exist: $chan\n"
    unless exists $channel{$chan};
  croak "Channel/Sink does not exist: $chan/$sink\n"
    unless exists $channel{$chan}{sinks}{$sink};
  $level = get_level($level);

  $channel{$chan}{sinks}{$sink}{level} = $level;
}

# -------------------------------------

=head2 add_sink_trans

Add a translator to a channel sink.

=over 4

=item ARGUMENTS

=over 4

=item chan

The channel to add the translator to.

=item sink

The sink to add the translator to.

=item trans

The translator to add.  The translator will be called in order after any
previously added (sink-specific) translators, all of which are called after
any channel translators, and will be given the results of the log string
having been through those translators.  The results of the translation
provided by this translator will be passed to any (sink-specific) translators
installed after this one.

=back

=back

=cut

sub add_sink_trans {
  my ($chan, $sink, $trans) = @_;

  croak "Channel does not exist: $chan\n"
    unless exists $channel{$chan};
  croak "Channel/Sink does not exist: $chan/$sink\n"
    unless exists $channel{$chan}{sinks}{$sink};
  croak sprintf("Translator for %s/%s not a subroutine: %s\n",
                $chan, $sink, ref $trans)
    unless UNIVERSAL::isa ($trans, 'CODE');

  push @{$channel{$chan}{sinks}{$sink}{trans}}, $trans;
}

# -------------------------------------
# PACKAGE FUNCTIONS
# -------------------------------------

=head1 PACKAGE FUNCTIONS

Z<>

=cut

sub get_level {
  my ($level) = @_;

  return
    unless defined $level;

  if ( $level !~ /^-?\d+/ ) {
    if ( exists LOG_LEVEL->{$level} ) {
      $level = LOG_LEVEL->{$level};
    } else {
      croak "unrecognized level: $level\n";
    }
  }

  return $level;
}

# -------------------------------------

=head2 Log

log a message

=over 4

=item ARGUMENTS

=over 4

=item channel

channel to log to

=item level

message log level.  Only if the log level is equal to or less than the channel
log level will it be logged.  For each sink, if the sink also has a level, the
message will be logged to that sink only if the message level is equal to or
below the sink level I<as well as> the channel level.

=item string

The string to log.  Do not append a line terminator; the sinks will do so
themselves if necessary.

=back

=back

=cut

sub Log {
  my ($channel, $level, $string) = @_;

  croak "Log::Info::Log : unrecognized channel: $channel\n"
    unless exists $channel{$channel};
  $level = get_level($level);

  if ( ! defined $string ) {
    my @caller = caller 1;
    $string =
      sprintf('Log::Info::Log *UNDEFINED* (called by %s::%s, at %s line %d)',
              @caller[0,3,1,2]);
  }

  my $details = $channel{$channel};

  return
    unless (( ! defined $channel{$channel}{level}
              or
              $level <= $channel{$channel}{level} )
            and %{$details->{sinks}}
            and grep({ my $sl = $details->{sinks}->{$_}->{level};
                       ! defined $sl || $level <= $sl; }
                     keys %{$details->{sinks}}));

  if ( exists $details->{trans} ) {
    my $i = 0;
    my @trans = @{$details->{trans}};
    while ( $i < @trans ) {
      eval {
        $string = $trans[$i]->($string);
      }; if ( $@ ) {
        warn ("Log::Info::Log : ",
              "Bad translation unit ($i) for channel $channel: $@\n");
      }
      $i++;
    }
  }

 SINK:
  while ( my ($name, $sink) = each %{$details->{sinks}} ) {
    next SINK
      if $name eq SINK_STDERR and $dying;

    my ($type, $sinklevel, $trans, $values) =
      @{$sink}{qw( type level trans values )};

    # Are we below the requisite level?
    next SINK
      if defined $sinklevel and $level > $sinklevel;

    # Any further translations?
    my $sinkstring = $string;
    if ( defined $trans ) {
      my $i = 0;
      my @trans = @$trans;
      while ( $i < @trans ) {
        eval {
          $sinkstring = $trans[$i]->($sinkstring);
        }; if ( $@ ) {
          warn (sprintf ("Log::Info::Log : Bad translation unit " .
                         "(%d) for channel %s sink %s: $@\n",
                         $i, $channel, $name));
        }
        $i++;
      }
    }

    if ( $type eq 'FILE' ) {
      _log_to_file   ($values, $sinkstring, $channel, $name, $level);
    } elsif ( $type eq 'FH' ) {
      _log_to_fh     ($values, $sinkstring, $channel, $name, $level);
    } elsif ( $type eq 'SUBR' ) {
      _log_to_subr   ($values, $sinkstring, $channel, $name, $level);
    } elsif ( $type eq 'SYSLOG' ) {
      _log_to_syslog ($values, $sinkstring, $channel, $name, $level);
    } else {
      warn ("Log::Info::Log : Bad sink type ($type) for channel/name ",
            "$channel/$name\n");
    }
  }
}

# -------------------------------------

=head2 Logf

=over 4

=item ARGUMENTS

=over 4

=item channel

As for L<Log|"Log">

=item level

As for L<Log|"Log">

=item format

As for L<sprintf/"sprintf">.

=item args

As for L<sprintf/"sprintf">.

=back

=back

=cut

sub Logf {
  my ($channel, $level, $format, @args) = @_;

  if ( ! exists $channel{$channel} ) {
    carp "Log::Info::Log : unrecognized channel: $channel\n";
    return;
  }

  if ( ! defined $format ) {
    my @caller = caller 1;
    Log ($channel, $level,
         sprintf('Log::Info::Logf: sprintf format not defined ' .
                 '(called by %s::%s, at %s line %d)',
                 @caller[0,3,1,2]));
    return;
  }

  if ( grep ! defined, @args ) {
    my @caller = caller 1;
    for (grep ! defined $args[$_], 0..$#args) {
      Log ($channel, $level,
           sprintf('Log::Info::Logf: format argument %s not defined ' .
                   '(called by %s::%s, at %s line %d)',
                   $_, @caller[0,3,1,2]));
      $args[$_] = '';
    }
  }

  Log ($channel, $level, sprintf $format, @args);
}

# Subroutines picked out from log to simplify things

sub _log_to_file {
  my ($values, $sinkstring, $channel, $name, $level) = @_;

  my ($logfn, $maxsize, $fh) = @{$values}{qw( fn maxsize fh )};
  local $/ = "\n"; chomp $sinkstring;
  $sinkstring .= "\n";

SIZE_CHECK:
  while (1) {
    if ( defined $fh ) {
      # Check if write to fh would take size past max; if so, close fh,
      # move name to unused old name, and undefine $fh to get new one
      # generated

      # tell() doesn't work for appended filehandles :-(
      my $fsize = (stat $fh)[7];
      my $new_size = $fsize + length $sinkstring;
      if ( $new_size > $maxsize and $fsize ) { # If this is this first
                                               # message, log it whatever
        $fh->close
          or warn("Log::Info::Log : ",
                  "Failure to close output log $logfn: $!\n");
        my ($dd, $mm, $yy) = (gmtime)[3..5];
        my $tname = sprintf ("%s-%d-%02d-%02d", $logfn,
                             $yy+1900, $mm+1, $dd);
        my $tail = '00';
        $tail++
          while -e join '-', $tname, $tail;
        rename $logfn, join '-', $tname, $tail
          or warn sprintf ("Log::Info::Log : " .
                           "Failure to rename output log %s to %s: $!\n",
                           $logfn, join '-', $tname, $tail);
        $fh = undef;
        delete $values->{fh};
      } else {
        last SIZE_CHECK;
      }
    }

    if ( ! defined $fh ) {
      # Open a shiny new fh, and assign it to fh
      if ( sysopen $fh, $logfn, O_WRONLY | O_APPEND | O_CREAT ) {
        $values->{fh} = $fh;
      } else {
        warn "Log::Info::Log : Couldn't open $logfn for appending: $!\n";
        delete_sink ($channel, $name);
        last SIZE_CHECK;
      }
    }
  }

  # Write the output!
  if ( defined $fh ) {
    $fh->syswrite($sinkstring)
      or warn sprintf ("Log::Info::Log : " .
                       "Print failed on file %s (name/chan %s/%s): $!\n",
                       $logfn, $name, $channel);
  }
}

# -------------------------------------

sub _log_to_fh {
  my ($values, $sinkstring, $channel, $name, $level) = @_;

  local $/ = "\n"; chomp $sinkstring;
  eval {
    $values->{fh}->syswrite("$sinkstring\n")
      or warn sprintf ("Log::Info::Log : " .
                       "Print failed on filehandle %s (channel %s): $!\n",
                       $name, $channel);
  }; if ( $@ ) {
    warn("Log::Info::Log : " .
         "Print to filehandle $name on channel $channel failed:\n  $@\n");
  }
}

# -------------------------------------

sub _log_to_subr {
  my ($values, $sinkstring, $channel, $name, $level) = @_;

  eval {
    $values->{subr}->($sinkstring);
  }; if ( $@ ) {
    warn("Log::Info::Log : " .
         "Invocation of subr $name on channel $channel failed:\n  $@\n");
  }
}

# -------------------------------------

sub _log_to_syslog {
  my ($values, $sinkstring, $channel, $name, $level) = @_;

  my $sysloglevel = LOG_NAME->{$level};

  if ( defined $values->{facility} ) {
    $sysloglevel = join '|', $values->{facility}, $sysloglevel;
  }

  if ( ! defined $sysloglevel ) {
    # Bump level up to next defined level
  LOG_LEVEL:
    foreach (LOG_LEVEL_VALUES) {
      if ( $_ < $level ) {
        $sysloglevel = LOG_NAME->{$_};
      } else { # $_ > $level
               # $_ != $level because ! defined $sysloglevel on loop entry
        last LOG_LEVEL; # LOG_LEVEL_VALUES is sorted; hence all successive
                        # values will also be > $level
      }
    }
  }

  if ( ! defined $sysloglevel ) {
    # Looks like none of the values are higher.  Default to LOG_EMERG.
    # call LOG_EMERG, then deref, just to check it's a valid level
    $sysloglevel = LOG_NAME->{LOG_EMERG()};
  }

  # Unset log mask
  my $oldmask = setlogmask (Sys::Syslog::LOG_UPTO(Sys::Syslog::LOG_DEBUG));
  syslog $sysloglevel, $sinkstring;
  setlogmask ($oldmask);
}


# -------------------------------------
# PACKAGE PROCEDURES
# -------------------------------------

=head1 PACKAGE PROCEDURES

Z<>

=cut

=head2 trap_warn_die

Add handlers to warn(), die(), to log messages to the log system.  Any
existing handlers are invoked after those added.

The die handler logs the message to C<CHAN_INFO> at C<LOG_ERR>.  The warn
handler logs the message to C<CHAN_INFO> at C<LOG_WARNING>.

This also traps C<Carp> messages.

=over 4

=item ARGUMENTS

I<None>

=back

=cut

sub import {
  my $class = shift;
  my (@bad_names, @export_symbols);
  my %export_ok = map({; $_ => 1 }
                      ':DEFAULT', @EXPORT, @EXPORT_OK,
                      map(":$_", keys %EXPORT_TAGS));
  for (@_) {
    if ( $_ eq ':trap' ) {
      __trap_warn_die();
    } elsif ( exists $export_ok{$_} ) {
      push @export_symbols, $_;
    } else {
      push @bad_names, $_;
    }
  }

  croak ("Arguments to " . __PACKAGE__ .
         " import  not recognized: ",
         join (', ', @bad_names), "\n")
    if @bad_names;

  $class->export_to_level(1, $class, @export_symbols);

}

my %redef_subr = (die => 1); # track of subrs intentionally redefined to
                             # exclude from warnings
sub trap_warn_die {
  Log(CHAN_INFO, LOG_WARNING,
      "trap_warn_die subr deprecated; use the import tag :trap instead\n");
  __trap_warn_die();
}

sub __trap_warn_die {

  my $lastmessage = '';

  my $package;
  {
    my $i = 0;
    do {
      ($package) = (caller($i))[0];
      $i++;
    } while ( $package eq __PACKAGE__ );
  }

  my $file = __FILE__;
  my $warnhook = $SIG{__WARN__};

  $SIG{__WARN__} = sub {
    # Nasty hack to avoid irritating mandatory redefine warnings bug
    if ( my ($subrname) = ($_[0] =~ /^Subroutine ([:\w]+) redefined at $file/ )
       ) {
      if ( exists $redef_subr{$subrname}             or
           ( index($subrname,':') == -1 and
             exists $redef_subr{"main::$subrname"} ) or
           ( $subrname =~ /^(?:main|CORE::GLOBAL)::([a-z_]\w+)$/ and
             exists $redef_subr{$1} )
         ) {
        return;
      }
    }
    my $message = join '', grep defined, @_;
    Log(CHAN_INFO, LOG_WARNING, $message);
    $warnhook->(@_)
      if defined $warnhook and UNIVERSAL::isa($warnhook, 'CODE');
  };

  my $save;

  my $diehook = $SIG{__DIE__};
  # Carp doesn't call die directly.  In know not how or why.  So this traps
  # calls to carp that didn't make it via the override
  $SIG{__DIE__} = sub {
    local $dying = 1;
    my $message = join '', grep defined, @_;
      if ( $message !~ /\A[\s\n]*\Z/ ) {
        Log(CHAN_INFO, LOG_ERR, $message)
          unless $message eq $lastmessage;
      }
    $diehook->(@_)
      if defined $diehook and UNIVERSAL::isa($diehook, 'CODE');
    $! = $save
      if $save;
  };

  # Override Carp messages if present
  for (qw( croak confess )) {
    no strict 'refs';
    my $subr_name = defined $package ? "${package}::$_" : "main::$_";
    my $subr = \&{$subr_name};
    if ( defined $subr ) {
      $redef_subr{$subr_name} = $redef_subr{$_} = 1;
      *{"$subr_name"} = sub {
        $save = $!+0;
        $subr->(@_);
      };
    }
  }

  *CORE::GLOBAL::die =
    sub {
      local $dying = 1;
      $save = $! + 0;
      my $message = join '', grep defined, @_;
      if ( $message !~ /\A[\s\n]*\Z/ ) {
        # Always terminate with a newline.  This ensures conformity of message
        # with that checked in SIG{__DIE__}, which otherwise may have an
        # "\n  at line..." appended.
        # If we want such appendages, we can add them ourselves
        $message =~
          s/([^\n])\z/sprintf("%s at %s line %d", $1, (caller)[1,2]) . "\n"/e;
        $message =~ s/\n+\z/\n/;
        Log(CHAN_INFO, LOG_ERR, $message)
          unless $message eq $lastmessage;
        $lastmessage = $message;
      }
      $! = $save
        if $save;
      CORE::die("$message");
    };

}

# -------------------------------------

=head2 enable_file_channel

Set up output channel (for string based command-line options).

=over 4

=item ARGUMENTS

=over 4

=item channel_name

name of the channel to log to.

=item fn

value of option presented by user.  If this option looks like a simple number,
it is treated as a log level (see below).  If this option looks like a simple
file name (C<m!^[A-Za-z0-9_.\\/-]+$>), it will be treated as an output file
(but output with the 'FH' type, so no auto-rotate, and special files will
work).  If this option looks like C<m!^:\d+!>, the numeric value will be
treated as a file descriptor, and output sent there.  If this value is
defined, but a blank string, then output will be sent to stderr.

If a value of the form C<\+\d+> precedes a file descriptor, or succeeds a
filename, then the numeric value is used to set the log level of the output
sink.  If not set, it defaults to C<LOG_INFO>, which is equivalent to C<+1>.
Hence, C<+0> is equivalent to C<LOG_INFO - 1>.

If this value is not defined, then no action is taken (this is to allow
compatibility with options processors, where a value is left undefined if its
option is never invoked).

If this value is defined but empty (C<''>), then the log level is set to
LOG_INFO (first time), and the output sent to STDERR.  If the option is seen
again, still with an empty string value, and with the same channel & sink
names, then the log level is increased one place.  This is to allow C<-v -v
-v>(or C<-vvv>)-style options.

=item option_name

name of the option invoked (used for error messages).

=item sink_name

the name of the sink to create.

=item term_progress

I<Optional>  If true, generate a sink with SINK_TERM_PROGRESS

=back

=back

=cut

my %seen_channel_sink;
sub enable_file_channel {
  my ($channel_name, $fn, $option_name, $sink_name, $term_progress) = @_;

  if ( defined $fn ) { # Else option not invoked
    $fn =~ s/\s*(.*?)\s*$/$1/;
    my $fh;

    my $level = LOG_INFO;
    if ( $fn =~ s/\+(\d+)// or $fn =~ s/^(\d+)$// ) {
      $level += $1-1;
    } else {
      my $key = join "\0", $channel_name, $sink_name;
      $level += $seen_channel_sink{$key}++;
    }

    if ( $fn eq '' ) {
      $fh = \*STDERR;
    } elsif ( substr($fn, 0, 1) eq ':' ) {
      my $fd = substr($fn, 1);
      if ( $fd =~ /^\d+/ ) {
        unless ( CORE::open $fh, ">&=$fd" ) {
          # Don't use Log::Info when the channels haven't opened...
          croak "Could not open file descriptor $fd for writing: $!\n";
        }
        select (((select $fh), $| = 1)[0]);
      } else {
        croak sprintf("Cannot handle non-integer file descriptor " .
                      "argument to %s: %s", $option_name, $fn);
      }
    } elsif ( $fn =~ m!^[A-Za-z0-9_.\\/-]+$! ) {
      unless ( CORE::sysopen $fh, $fn, O_CREAT | O_EXCL | O_WRONLY ) {
        croak "Could not open file $fn for (create &) writing: $!\n";
      }
    } else {
      croak "Cannot handle argument to $option_name: $fn\n";
    }

    if ( defined $fh ) {
      if ( $term_progress ) {
        add_sink($channel_name, $sink_name, SINK_TERM_PROGRESS($fh, $level));
      } else {
        add_sink($channel_name, $sink_name, 'FH', $level, { fh => $fh });
      }
    }
  }
}

# ----------------------------------------------------------------------------

=head1 EXAMPLES

Z<>

=head1 BUGS

=over 4

=item *

C<%m> strings will get expanded to $! in C<SYSLOG> sinks; this is a bug, and
may get fixed at any time.

=back

=head1 REPORTING BUGS

Email the author.

=head1 AUTHOR

Martyn J. Pearce C<fluffy@cpan.org>

=head1 COPYRIGHT

Copyright (c) 2001, 2002, 2003, 2005 Martyn J. Pearce.  This program is free
software; you can redistribute it and/or modify it under the same terms as
Perl itself.

=head1 SEE ALSO

Z<>

=cut

1; # keep require happy.

__END__
