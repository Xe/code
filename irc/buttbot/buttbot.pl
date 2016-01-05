#!/usr/bin/perl

package main;

use strict;
use warnings;
use Data::Dumper;

my $conf_file = $ARGV[0] || "./conf.yml";
my $bot = BasicButtBot->new(config => $conf_file);

# fly, my pretties, fly!
$bot->run;

package BasicButtBot;

use base qw/Bot::BasicBot/;

# What would you like to Butt today?
use Butts;
# config-parsing is a bit passe.
use YAML::Any;
use Data::Dumper;
# so we can hax our own handlers for things.
use POE;

sub init {
    my $self = shift;

    $self->{settings}->{friends} = {};
    $self->{settings}->{enemies} = {};

    $self->load_config(0);

    $self->{authed_nicks} = {};
    $self->{in_channels} = {};

    # TODO: should we pass more options in?
    $self->{butter} = Butts->new(meme => $self->config('meme'));

    if ($self->config('debug')) {
        $self->log("DBG: Debugging output enabled\n");
    }

    1;
}

sub load_config {
    my ($self, $reload) = @_;
    $reload = 0 unless defined $reload;

    my $config = YAML::Any::LoadFile($conf_file);

    # only load these settings at startup.
    unless ($reload) {
        $self->{$_} = $config->{connection}->{$_}
          for (keys %{$config->{connection}});
    }

    my ($old_friends, $old_enemies)
      = ($self->{settings}->{friends},
         $self->{settings}->{enemies});

    $self->{settings}->{$_} = $config->{settings}->{$_}
      for (keys %{$config->{settings}});

    # merge the old copies with the new ones (in case we're reloading)

    $self->{settings}->{friends}->{keys %$old_friends}
      = values %{$old_friends};
    $self->{settings}->{enemies}->{keys %$old_enemies}
      = values %{$old_enemies};
}

#@OVERRIDE
sub start_state {
    # in ur states, adding extra events so we can invite and shiz.
    my ( $self, $kernel, $session ) = @_[ OBJECT, KERNEL, SESSION ];
    my $ret = $self->SUPER::start_state($self, $kernel, $session);
    $kernel->state('irc_invite', $self, 'handle_invite');
    $kernel->state('irc_405', $self, 'handle_err_too_many_chans');

    return $ret;
}

sub handle_err_too_many_chans {
    my ($self, $server, $msg_text, $msg_parsed)
      = @_[OBJECT, ARG0, ARG1, ARG2];
    $self->log("IRC: too many channels:\n" . Dumper($msg_parsed) . "\n");
    # TODO: how can we let the user who requested us know that we're
    # unable to comply?  Maybe keep a queue of pending commands, and
    # only respond ok/err when we get an appropriate response from server.
    return;
}

sub handle_invite {
    my ($self, $inviter, $channel) = @_[OBJECT, ARG0, ARG1];
    $inviter = $self->nick_strip($inviter);
    if ($self->config_bool('invite')) {
        $self->log("IRC: Going to join $channel, invited by $inviter\n");
    } else {
        $self->pm_reply($inviter, "Sorry, inviting is disabled by the admin.");
        $self->log("IRC: invite refused from $inviter to $channel\n");
    }
    $self->join_channel($channel);
}

sub join_channel {
    my ($self, $channel, $key) = @_;
    $key = '' unless defined $key;
    $self->log("IRC: Joining channel [$channel]\n");
    $poe_kernel->post($self->{IRCNAME}, 'join', $channel, $key);
}

sub leave_channel {
    my ($self, $channel, $part_msg) = @_;
    $part_msg ||= "ButtBot Go Byebye!";
    $self->log("IRC: Leaving channel [$channel]: \"$part_msg\"\n");
    $poe_kernel->post($self->{IRCNAME}, 'part', $channel, $part_msg);
}

sub change_nick {
    my ($self, $new_nick) = @_;
    $poe_kernel->post($self->{IRCNAME}, 'nick', $new_nick);
    $self->log("IRC: changing nick to $new_nick\n");
}

sub in_channel {
    my ($self, $channel, $present) = @_;
    if (defined $present) {
        if (!$present) {
            delete $self->{in_channels}->{$channel}
              if exists $self->{in_channels}->{$channel};
        } else {
            $self->{in_channels}->{$channel} = 1;
        }
    }
    return $self->{in_channels}->{$channel};
}

sub get_all_channels {
    my ($self) = @_;
    return keys %{ $self->{in_channels} };
}


sub nick_change {
    my ($self, $from, $to) = @_;

    if ($self->is_me($from)) {
        $self->{nick} = $to;
        $self->log("IRC: changed own nick from $from to $to\n");
    }
    return;
}

sub chanjoin {
    my ($self, $ref) = @_;
    my ($channel, $who) = @{$ref}{qw/channel who/};
    $self->log("IRC: [$channel] $who joined\n");

    if ($self->is_me($who)) {
        $self->in_channel($channel, 1);
    }
    return;
}

sub chanpart {
    my ($self, $ref) = @_;
    my ($channel, $who) = @{$ref}{qw/channel who/};
    $self->log("IRC: [$channel] $who left\n");

    if ($self->is_me($who)) {
        $self->in_channel($channel, 0);
    }
    return;
}

sub kicked {
    my ($self, $ref) = @_;
    my ($channel, $who, $who_by, $why) =
      @{$ref}{qw/channel kicked who reason/};

    $who_by = $who_by || "<UNKNOWN>";
    $self->log("$who just got kicked from $channel by $who_by: \"$why\"\n");
    if ($self->is_me($who)) {
        $self->in_channel($channel, 0);
    }
    return;
}

# TODO: refactor these 3 better. Emote should never have to deal with commands
# or prefixes.  Just a message to be re-butted.
sub emoted {
    my ($self, $ref) = @_;
    $self->handle_said_emoted($ref, 1);
}

sub said {
    my ($self, $ref) = @_;
    $self->handle_said_emoted($ref, 0);
}

sub handle_said_emoted {
    my ($self, $ref, $reply_as_emote) = @_;
    # slicin' ma hashes.
    my ($channel, $body, $address, $who) =
      @{$ref}{qw/channel body address who/};

    # address doesn't even get set unless it's true :(
    $address ||= 0;

#    print STDERR Dumper($ref);
#    print STDERR "\n---------\n";

    if ($channel ne 'msg') {
        # address is what is stripped off the front 
        my $addressed = $address && $address ne 'msg';
        # normal command
        # eg: <bob> ButtBot: stop it
        return if $self->handle_channel_command($who,
                                                $channel,
                                                $body,
                                                $addressed);
    } elsif ($channel eq 'msg') {
        # parse for command
        return if $self->handle_pm_command($who, $body);
    }

    # butting is the default behaviour.
    $self->log("BUTT: Might butt\n");
    if ($self->to_butt_or_not_to_butt($who, $body)) {
        $self->log("BUTT: Butting $who in [$channel]\n");
        $self->buttify_message($who, $channel, $body, $reply_as_emote, 0);
    }

    return;
}

sub parse_command {
    my ($self, $msg, $require_prefix) = @_;
    my $cmd_prefix = quotemeta($self->config('cmd_prefix'));

    $require_prefix = 1 unless defined $require_prefix;
    if (!$require_prefix) {
        $cmd_prefix .= '?';
    }

    if ($msg =~ m/^$cmd_prefix([\w_-]+)\s*(.*)$/) {
        return ($1, $2);
    } else {
        return ();
    }
}

sub _parse_channel {
    # parse a string into a channel (optionally with a leading # or &), and the
    # remainder of the string.
    my ($str) = @_;
    if ($str =~ m/^([#&]?)([^,\s\x07]+)\s*(.*)$/) {
        return ($1.$2, $3) if $1;
        return ('#'.$2, $3);
    }
    return (undef, $str);
}

sub pm_reply {
    my ($self, $who, $msg) = @_;
    $self->say(who => $who, channel => 'msg', body => $msg);
}

# TODO: handle de-authentication when nick changes(?) or leaves all shared
# channels.
sub handle_pm_command {
    my ($self, $who, $msg) = @_;

    $self->log("CMD: testing for PM command: [$who], [$msg]\n");

    my ($cmd, $args) = $self->parse_command($msg,0);
    return 0 unless defined $cmd && length $cmd;
    $self->log("CMD: [$msg] is a PM command\n");

    # commands that don't need authentication
    # NB: They need to call return or they'll hit the auth barrier.
    if ($cmd eq 'auth') {
        if ($args eq $self->config('pass')) {
            $self->auth_set($who, 1);
            $self->pm_reply($who, "Hello again!");
        } else {
            $self->pm_reply($who, "Authentication Failed :(");
        }
        return 1;
    } elsif ($cmd eq 'friend') {
        # TODO: become friend/enemy
    } elsif ($cmd eq $self->config('meme')) {
        my $ret = $self->buttify_message($who, 'msg', $args, 0);
        if (!$ret) {
            $self->pm_reply($who, "Sorry, can't ".$self->config('meme'));
        }
        return 1;
    }

    # do some authentication
    unless ($self->is_authed($who)) {
        $self->pm_reply($who, "You're not authenticated :(");
        return 1;
    }

    # TODO: command to query/set butt frequencies?


    # commands that *do* need authentication
    if ($cmd eq 'join') {
        my ($arg_chan, $arg_rem) = _parse_channel($args);
        if (defined $arg_chan) {
            if ($self->in_channel($arg_chan)) {
                $self->pm_reply($who, "I'm already in that channel!");
            } else {
                $self->join_channel($arg_chan);
                $self->pm_reply($who, "Joining channel $1");
            }
        } else {
            $self->pm_reply($who, "I needs a channel name please.");
        }

    } elsif ($cmd eq 'leave') {
        my ($arg_chan, $arg_msg) = _parse_channel($args);
        if (defined $arg_chan) {
            if (!$self->in_channel($arg_chan)) {
                $self->pm_reply($who, "I'm not in that channel!");
            } else {
                $self->leave_channel($arg_chan, $arg_msg);
                $self->pm_reply($who, "Ok.");
            }
        } else {
            $self->pm_reply($who, "I needs a channel name please. "
                            . "Also maybe a message.");
        }

    } elsif ($cmd eq 'change-nick') {
        unless ($self->config_bool('changenick')) {
            $self->pm_reply($who, "Sorry, changing nicks is disabled.");
            return 1;
        }
        if ($args =~ m/^(\w+)/) {
            $self->change_nick($1);
            $self->pm_reply($who, "Ok.");
        } else {
            $self->pm_reply($who, "Can't change it to that, boss");
        }
    } elsif ($cmd eq 'set-meme') {
        unless ($self->config_bool('set_meme')) {
            $self->pm_reply($who, "Changing the meme is disabled, sorry");
            return 1;
        }
        if ($args =~ m/^(\w+)/) {
            my $old_meme = $self->config('meme');

            $self->config('meme', $1);
            $self->{butter}->meme($1);

            $self->pm_reply($who, "Changed meme from [$old_meme] to [$1]");
        } else {
            $self->pm_reply($who, "Meme unchanged. Learn some syntax");
        }
    } elsif ($cmd eq 'deauth') {
        $self->auth_set($who, 0);
        $self->pm_reply($who, "Ok. See you again sometime");
    } elsif ($cmd eq 'channel-list') {
        my @channels = $self->get_all_channels;;
        $self->pm_reply($who, "I'm in: " . join(', ', @channels));
    } elsif ($cmd eq 'reload') {
        unless ($self->config_bool('reload')) {
            $self->pm_reply($who, "Reloading is disabled.");
            return 1;
        }
        # reload settings, but not connection info.
        $self->load_config(1);
        $self->pm_reply($who, "Config reloaded");

    } else {
        $self->pm_reply($who, "Dunno what you want.");
    }

    return 1;
}

sub handle_channel_command {
    my ($self, $who, $channel, $msg, $addressed) = @_;
    # return false if we don't handle a command, so things can
    # be appropriately butted.

    $self->log("CMD: testing user command\n");
    # if we were addressed (as <foo> BotNick: CMD), don't require
    # the command prefix char.  Otherwise do.
    my ($cmd, $args) = $self->parse_command($msg, $addressed?0:1);
    return 0 unless defined $cmd && length $cmd;

    if ($cmd eq $self->config('meme')) {
        $self->buttify_message($who, $channel, $args, 0,1);
        return 1;
    }

    return 0;

    # TODO: !stopit - adds them to the enemies list.
    # TODO: !butt - randomly butts something?
}

sub buttify_message {
    my ($self, $who, $where,
        $what, $reply_as_emote,
        $prefix_addressee) = @_;

    my $meme = $self->config('meme');

    $prefix_addressee = 0 unless defined $prefix_addressee;
    $reply_as_emote = 0 unless defined $reply_as_emote;

    my $butt_msg = $self->{butter}->buttify_string($what);

    unless ($self->_was_string_butted($what, $butt_msg)) {
        $self->log("BUTT: String \"$butt_msg\" wasn't butted");
        return 0;
    }

    unless ($butt_msg eq $meme) {
        if ($reply_as_emote) {
            $self->emote(channel => $where, who => $who,
                       body => $butt_msg, address => 0);
        } else {
            $self->say(channel => $where, who => $who,
                       body => $butt_msg, address => $prefix_addressee);
        }
    } else {
        $self->log("BUTT: butting resulted in solo butt and was discarded.");
    }

    return 1;
}

sub to_butt_or_not_to_butt {
    my ($self, $sufferer, $message) = @_;
    my $rnd_max = 0;
    my $frequencies = $self->config('frequency');

    return 0 if $self->might_be_a_bot($sufferer);

    # Fixes issue 6.
    unless ($self->_is_string_buttable($message)) {
        $self->log("BUTT: String is not buttable");
        return 0;
    }

    if ($self->is_enemy($sufferer)) {
        $self->log("BUTT: [$sufferer:enemy] not butting\n");
        return 0;
    } elsif ($self->is_friend($sufferer)) {
        $rnd_max = $frequencies->{friend};
        $self->log("BUTT: [$sufferer:friend] prob is 1/$rnd_max\n");
    } else {
        $rnd_max = $frequencies->{normal};
        $self->log("BUTT: [$sufferer:normal] prob is 1/$rnd_max\n");
    }
    my $rnd = int rand $rnd_max;
    return ($rnd==0);
}

# FIX for
# http://code.google.com/p/buttbot/issues/detail?id=6
# Message must contain at least some word characters that we can butt.
sub _is_string_buttable {
    my ($self, $str) = @_;
    return $str =~ m/[a-zA-Z]+/;
}

# test if a string is the same as it was pre- and post-butting.
# returns true if strings are different
sub _was_string_butted {
    my ($self, $in, $out) = @_;
    my $meme = $self->config('meme');

    # we can't trust whitespace, since we might have trimmed it differently.
    $in =~ s/\s+//g;
    $out =~ s/\s+//g;
    return (lc($in) ne lc($out));
}

sub might_be_a_bot {
    my ($self, $who) = @_;
    return ($who =~ m/cout|(?:bot$)/i);
}

sub is_enemy {
    my ($self, $who) = @_;
    my $enemies = $self->config('enemies');
    return exists $enemies->{$who}
}

sub is_friend {
    my ($self, $who) = @_;
    my $friends = $self->config('friends');
    return exists $friends->{$who}
}

sub is_me {
    my ($self, $who) = @_;
    # TODO: support B::BBot's alt_nicks param too?
    return $self->{nick} eq $who;
}

sub config {
    my ($self, $key, $value) = @_;
    if (defined $value) {
        $self->{settings}->{$key} = $value;
    }

    $self->log("CFG: $key requested doesn't exist\n")
      unless exists $self->{settings}->{$key};

    return $self->{settings}->{$key};
}

sub config_bool {
    # types :(
    my ($self, $key, $value) = @_;
    if (defined $value) {
        $self->{settings}->{$key} = $value?1:0;
    }
    my $val = $self->{settings}->{$key} || 0;
    if ($val =~ m/(?:[tT]rue)|(?:[Yy]es)|1/) {
        return 1;
    } else {
        return 0;
    }
}

sub is_authed {
    my ($self, $nick) = @_;
    return exists($self->{authed_nicks}->{$nick});
}

sub auth_set {
    my ($self, $nick, $auth) = @_;
    if ($auth) {
        $self->{authed_nicks}->{$nick} = 1;
    } else {
        if ($self->is_authed($nick)) {
            delete($self->{authed_nicks}->{$nick});
        } else {
            $self->log("Trying to de-auth someone who isn't authenticated: $nick\n");
        }
    }
}

sub log {
    my $self = shift;
    if ($self->config_bool('debug')) {
        $self->SUPER::log(@_);
    }
}
1;

