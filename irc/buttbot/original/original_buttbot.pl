#!/usr/bin/perl 
use strict;
use warnings;

use Butts qw(buttify);
use IO::Socket;

$|++;

my %CONF;

&readconf(@ARGV);

my $socket = new IO::Socket::INET(
    PeerAddr => $CONF{server},
    PeerPort => $CONF{port},
    proto    => 'tcp',
    Type     => SOCK_STREAM,
    Timeout  => 10
) or die "socket: $!";

_send("NICK $CONF{nick}");
_send("USER $CONF{ident} 0 * :$CONF{gecos}");

_fork() unless $CONF{debug};

my $auth = "";

#list of friends (people who get buttified more often) and enemies (people who dont get butted.)
my (%friends, %enemies);
#frequency that normal people and friends get butted
my ($normalfrequency, $friendfrequency);
#last thing said by someone in the channel
my (@previousdata);
my ($previouschannel);
my (%linestotal);
my (%timeoflastbutting);


#pre-setting frequencies
$friendfrequency = $CONF{friendfreq} || 37;
$normalfrequency = $CONF{normfreq} || 51;

#The meme.
my $meme = $CONF{meme} || "butt";

#remove whitespace!
$CONF{channel} =~ s/\s+//;

# add friends from conf file
%friends = map {
    (my $friend = $_) =~ s/^\s+|\s+$//g;

    $friend, 1;
} split /,/, $CONF{friends} if $CONF{friends};

# add enemies from conf file
%enemies = map {
    (my $enemy = $_) =~ s/^\s+|\s+$//g;

    $enemy, 1;
} split /,/, $CONF{enemies} if $CONF{enemies};

#== forever butting... ========================================================

process() while 1;

#== subroutines ===============================================================

sub process {
    die "main: $!" if $!;

    process_line($_) for split /\n/, gets();
}

sub cmd_pong {
    my $command = shift @_;
    pong($command =~ /^:\d+$/ ? $command : "$CONF{nick} $command")
}

sub process_line {
    my $line = shift;
    print "$line\n";

    my ($from, $command, @data) = split /\s+/, $line;

    $from    = defined $from    ? $from    : '';
    $command = defined $command ? $command : '';

    die "from server: @data" if $from eq 'ERROR';

    # if server pings, ping back.

    if ($from eq 'PING') {
        cmd_pong($command);
    }

    # If buttbot has successfully connected to the server, join a channel.
    if ($command eq '001') {
        cmd_connect();

    # otherwise, if it's a message
    } elsif ($command eq 'PRIVMSG') {
        cmd_privmsg($from, @data);

    } elsif ($command eq 'INVITE' && $CONF{'invite'} ) {
        my $c = $data[1];
        $c =~ s/^://;
        _send("JOIN $c");
    }
        
}

sub cmd_connect {
    _send("MODE $CONF{nick} -x"); # hiding hostnames is for wimps.
    if (defined $CONF{channel}) {
        _send("JOIN $CONF{channel}") ;
    }

    if (defined $CONF{nickpass})
    {
        _send("NICKSERV :identify $CONF{nickpass}");
    }
}

sub cmd_privmsg {
    my($from, @data) = @_;

    # get destination of message
    my $to = shift @data;

    # get first word of message (might be command)
    my $sub = shift @data;

    ## remove preceding ':'
    $sub =~ s/^://;

    # if a user private messages the bot...
    if ($to eq $CONF{nick}) {
        pm_bot($from, $sub, @data);

    #if messages come from channel, start buttifying
    } elsif ($to =~ /^\#/) {
        pm_channel($from, $to, $sub, @data);
    
    }
}

sub pm_bot {
    my ($from, $sub, @data) = @_;

    my $to = $from;

    $to =~ s/^:(.*)!.*$/$1/;
    #If the command is !butt, buttify message.
    
    ##if the first word in the string is equal to the password, set the user to be the admin
    if ($sub eq "!auth" && $data[0] eq $CONF{pass}) {
        $auth=$from;
        _send("PRIVMSG $to :Authed.");
    } elsif ($sub eq "!$meme" and @data >0 ) {
	my @bread_and = &buttify($meme, @data);
	# comparing lists is piss easy in python :(
	my $jam = join(" ", @data);
	my $cock = join(" ", @bread_and);
	_send("PRIVMSG $to :$cock") if ($jam ne $cock); 
    }

    ##ADMIN FUNCTIONS
    if ($auth eq $from)  {
        if ($sub eq "!join" and @data > 0) {
            $CONF{channel} = $CONF{channel}.",";
            $CONF{channel} = $CONF{channel}.$data[0];
            _send("JOIN $data[0]");
        } elsif ($sub eq "!leave" and @data > 0) {
            $CONF{channel} =~ s/$data[0]//;
            _send("PART $data[0]");
        } elsif ( $sub eq "!meme" and ($CONF{setmeme} ne "no") and @data >0 ) {
            $meme = $data[0];
            _send("PRIVMSG $to :Meme changed to $meme.");
            if ($CONF{changenick} ne 'no') {
                $CONF{nick} = $meme."bot";
                _send("NICK :$CONF{nick}");
            }
        }
    }
}

sub pm_channel {
    my ($from, $to, $sub, @data) = @_;

    my $sender = $from;

    $sender =~ s/^:(.*)!.*$/$1/;
    if (exists $linestotal{$to}) {
        $linestotal{$to}++;
    } else {
        $linestotal{$to} = 1;
    }

    ##ignores statements from cout and users containing the word "bot"
    if (($from !~/^:cout/) && ($from !~/^:[^!]*bot[^!]*!/i)) {
        if ($sub !~ /^!/ && ($sub !~ /^\./)) {
            my $rnd = 1;
            unshift (@data,$sub);
            if (@data > 1) {
                #if it's a enemy, don't buttify message. If friend, buttify message more often.
                $rnd = tobuttornottobutt($sender);
            }
                  
            #if the random number is 0, buttify that data
            if ($rnd ==0) {
                $timeoflastbutting{$to} = time;
                sleep(@data*0.2 + 1);
                # if the message is a CTCP line, avoid replacing
                # the CTCP command in the first word
                if (substr($data[0], 0, 1) eq "\1") {
                    # only butt if the command is not the only word
                    if (@data > 1 && $data[1] ne "\1") {
                        my $first = shift(@data);
                        my @butted = &buttify($meme, @data);
                        unshift(@butted, $first);

                        my $jam = join(" ", @data);
                        my $cock = join(" ", @butted);
                        _send("PRIVMSG $to :$cock") if ($jam ne $cock); 
                    }
                } else {
                     my @bread_and = &buttify($meme, @data);
                     # comparing lists is piss easy in python :(
                     my $jam = join(" ", @data);
                     my $cock = join(" ", @bread_and);
                     _send("PRIVMSG $to :$cock") if ($jam ne $cock); 
                }
            }
        } elsif ($sub eq "!$meme" && @data >0 ) {
            if (($data[0] !~ /^!/) && ($data[0] !~ /^cout/)) {
                my @bread_and = &buttify($meme, @data);
                # comparing lists is piss easy in python :(
                my $jam = join(" ", @data);
                my $cock = join(" ", @bread_and);
                if ($CONF{buttcommand} ne 'no') {
                  _send("PRIVMSG $to :$cock") if ($jam ne $cock); 
                } else {
                  _send("PRIVMSG $sender :$cock") if ($jam ne $cock); 
                }
            }
        }
    }
}

#for future determining of butting
sub tobuttornottobutt
{
    my($rnd, $sender);
    $sender = shift;
    if (exists $enemies{$sender}) {
        $rnd = 1;
    } elsif (exists $friends{$sender}) { 
        $rnd = int(rand(int($friendfrequency)));
    } else {
        $rnd = int(rand(int($normalfrequency)));
    }
    return $rnd;
}

sub gets {
  my $data = "";
  $socket->recv($data, 1024);

  return $data;
}

sub pong {
    _send("PONG $_[0]");
}

sub _send {
  $socket->send("@_\n");
}

sub _fork {
    my $spoon = fork;

    if (defined $spoon) {
        if ($spoon == 0) { # is child process
            return;
        } else {
            print "exiting, child pid = $spoon\n";
            exit;
        }
    } else {
        die "fork: $!";
    }
}

sub readconf {
    my $file = shift;
    ($file = $0) =~ s/\.pl$/\.conf/i unless defined $file;

    open my($fh), $file or die "readconf: cannot open $file";

    while (<$fh>) {
        next if /^\#/;

        if (/^\s*([^\s]+)\s*=\s*(.+)$/) {
            $CONF{lc($1)} = $2;
        }
    }
}
