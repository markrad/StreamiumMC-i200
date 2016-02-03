#!/usr/bin/perl -w
# streamium2d 1.0g

# Credit and thanks given to Nathan Peterson for his work in hacking the
# protocol.  Some of his original pclink.pl code used here.  Visit Nathan's
# website at http://www.siteswap.org/streamium/

# Credit and thanks also given to Dave Witt for the script that I used as a 
# basis to this script. Some of his code can still be seen in here. Visit 
# Dave's website at http://www.witt.tv/streamiumd

# Description:
#
# This program is designed to feed your Streamium MC-i200 with MP3s from your
# collection. Philips, in their wisdom, implemented a non-standard UPNP in that
# device even though the box said it was UPNP. This forced the user to use the
# (rather lame) Philips software to feed it. Having realised that the only 
# reason I was running a Windows box was to run that piece of software I 
# decided to look around for a Linux alternative. I came across Dave Witt's 
# code, tried it and it worked but for my usage it had some drawbacks.
#
# 	1) It did not read the ID3 tag information but rather relied upon a 
#	   directory structure.
#	2) It would not give me albums, genres and songs on my Streamium 
# 
# So this is my implementation of the same thing. It will provide the
# Streamium with the option of Artist, Albums, Genres or songs. Each artist, as
# with the orginial Philips software, will expand into a list of their albums 
# and 'all songs'. It should happily talk to multiple clients but I haven't 
# tested it. It also supports the super scroll invoked by using the remote to
# enter a letter and skip to the earliest in the list that begins with it.

# I've run this on Ubuntu Gusty Gibbon and Debian Etch. You may need some Perl
# modules from CPAN. Sys::HostIP was not installed on either of my systems.
#
# Currently I run this script on a Synology NAS server.
# 
#            *** READ THIS EVEN IF YOU READ NOTHING ELSE ***
# Unfortunately none of the available perl packages for reading MP3 tags 
# would work with all of my MP3s. I tried three and all of them had some 
# problem with some tags so I wrote my own code. Hopefully this situation will
# improve because I don't really want to maintain the tag reading code. Your
# results may vary.
# 
# Comments and feedback are welcome.  If you like this software, let me know!
#
# By the way I am not a particularly experienced Perl programmer so if you 
# have a problem with the way I've coded things just be nice.
#
# To do:
# * Find an MP3 package that works
# * Possibly add support for a UPNP backend instead of the file system - I'm 
#   thinking about that. It can be done, I've seen a script for it.
#	Add ShoutCast support
#	Treat 'Various' albums differently
#
# Version 1.0.a: May 03 2008
#	Released:	No
#	Pretty much a rewrite so gave it a new name and version
#
# Version 1.0.b: August 23 2008
#	Released:	Yes
#	Initial release
#
# Version 1.0.c: August 24 2008
#	Released: 	Yes
#	Moved indexing of songs to run after daemonizing to speed boot up on
#	systems that load it as part of the initialization. (I do and the old 
#	version sucked)
#
# Version 1.0.d: September 08 2008
#	Released:	Yes
#	Added the option to serve songs from an external web server and suppress
#	the running of the internal web server. See option --urlprefix.
#	Centralized the handling of configuration options by putting them into a
#	hash and passing it to those who need it. 
#	Massively reduced dependancy on global variables. 
#	Deleted unused variables.
#	Added persistent index on disk to speed start up with rebuild option.
#	Added support for playlists.
#
# Version 1.0.e: October 30 2008
#	Released:	Yes
#	Added all of the ID3V1 standard genres to the genre interpreter.
#	Added playlist support
#	Added random song & random album support. 
#		This made a fairly radical 
#		change to the UI on the Streamium. It will only support a maximum of
#		five top level nodes. Node five is now Extras with sub-nodes of 
#		Playlists and Randoms.
#	Fixed code to generate valid XML
#	Ensured text from ID3 tags is decoded properly
#	Write out PID file for easier daemon control
#	Kill web server when main is terminated
#	Canned the idea of a list of played files. Too difficult to implement in 
#		my design for the value it gives.
#	
# Version 1.0.f: March 24 2009
#	Released:	Yes
#	Fixed index file option bug
#	Added 'index and quit' option
#	Made options case sensitive
#	Modified pidfile option to -Pidfile
#
# Version 1.0.g: February 01 2012
#	Released:	Yes
#	Fixed bug that occured when there were no playlists to index
#	Modified code to use functionality available on a Synology NAS device
sub version
# ---------------------------------------------------------------------------- #
# version:
#	Returns script version
#
#	returns:	Script version string
# ---------------------------------------------------------------------------- #
{
	# May be this should just be a constant...?
	return "1.0.g";
}

sub displayUsage
# ---------------------------------------------------------------------------- #
# displayUsage:
#	Displays usage information and exits.
#
#	returns:	Nothing
# ---------------------------------------------------------------------------- #
{
	my $version = version();
	print <<EOF;
	
streamium2d version $version

Usage: $0 options /your/mp3/root/directory

Options:
 -f (--foreground)

    Run in the foreground rather than as a deamon.

 -p (--port) portNum

    portNum specifies the port that the embedded web server is to use. If
    this is not specified it will default to 8080.


 -s (--servername) serverName

    This will name that will display on the Streamium when the PC Link is
    started. It enables the end user to choose between multiple servers. If
    you only run one server then the default name is probably adequate. The
    default is streamium2d + version

 -i (--indexfile) <indexFileName>

    This informs the process that an index file should be used. If the index
    does not already exist then this will implicitly turn on the -b option.
    See below. If it does exist then the index of songs will be loaded from
    the specified file unless -b has been specified. By default the index
    will be read from or built at /var/run/streamium2d/streamium2d.index. You
    will need to create this directory and ensure the user that you use to run
    the daemon has write permission to this directory should you choose to use
    the default.

 -x (--xit)
 
    This will implicitly turn on the -b option. After the index has been built
    the process will exit. This is to allow the index to be rebuilt and then
    restart the daemonized process which will then pick up the new index. This 
    option will also force on foreground processing.
    
 -b (--buildindex)

    Tells the process to rebuild the index of songs and playlists. This will
    not implicitly turn on the -i option. See above. If -i is not specified
    then this is the default action. It may also happen if -i is specified
    but the index file either does not exist or was built with a different
    version of the script. It's recommended that an indexfile is used at
    boot time and a cron job can shutdown and restart the server with an
    index build at a convenient time.

 -u (--urlprefix) urlPrefix

    This option implicitly prevents the internal web server from running.
    This value will replace the root directory at the front of the file name
    with this string. Use full syntax, for example 'http://songserver/CDs/'.
    Also see examples below.

-w (--webgaol)

    The internal web server will only return files from the mp3 root
    directory tree. This also includes playlist entries unless they specify
    a fully qualified URL, that is the name begins with http://.

-P (--Pidfile)

    The name of the file to write containing the daemonized process' pid. This 
    can be used to kill the process later. If this is not specified it will 
    default to /var/run/streamium2d/streamium2d.pid.
    
 -d (--debug)

    Display debug messages to STDOUT (not extensive!).

 -v (--version)

    Display version number and exit.

-h (--help)

    Display this information and exit.

Notes:
 If you do not specify -w then the server will be capable of serving any file
 that it has read access to. This may not be what you want! If you do specify it
 though, any playlist will have to reference songs within your root directory
 tree. Relative path names will be assumed to be relative from the root
 directory tree.

Example:
 Start the server so Streamiums will see "Edgars Music", and can
 browse+play mp3s under /home/eapoe/mp3s, with the http server dishing
 out files on http port 8081:

    $0 -s "Edgars Music" -p 8081 /home/eapoe/music

The same, but watch it in action:

    $0 -f -d -s "Edgars Music" -p 8081 /home/eapoe/music

Only allow it access to files at or below /home/eapoe/Music

    $0 -w -s "Edgars Music" /home/eapoe/music

Use an index:

    $0 -i /home/eapoe/streamium/mci250.index /home/eapoe/Music

Use an index but force an index rebuild:

    $0 -b -i /home/eapoe/streamium/mci250.index /home/eapoe/Music

Using an external web server:

    $0 -u "http://songserver/CDs/ /home/eapoe/music

A song at /home/eapoe/music/Aerosmith/Pump/01 Young Lust.mp3 will be sent to
the Streamium with the URL http://songserver/CDs/Aerosmith/Pump/01 Young Lust.mp3
EOF
	exit;
}

use warnings;
use strict;
use Encode;
use Socket;
use Storable;
use English;
use Getopt::Long;
use POSIX qw(setsid);
use POSIX ":sys_wait_h";
use IO::Socket;
use IO::Select;
use Net::hostent;
use Fcntl ':seek';
use Sys::Hostname::Long;
#use Sys::HostIP;
use File::Find;
use File::Basename;
use Data::Dumper;

use constant UPD_BROADCAST_PORT		=> 42591;
use constant LISTENER_PORT			=> 42951;
use constant SCRIPT_VERSION			=> '1.0a';
use constant VENDOR_ID				=> 'Streamium2d';
use constant DEFAULT_INDEXFILENAME	=> '/var/run/streamium2d/streamium2d.index';
use constant DEFAULT_PIDFILENAME	=> '/var/run/streamium2d/streamium2d.pid';

$SIG{__WARN__} = sub { die @_ };
$SIG{INT} = \&endprog;
$SIG{CHLD} = 'IGNORE';    # auto-reap the zombies

# Globals
my $debugFunc = sub {};
my $webServerPid = 0;

# Locals
my $config = {};
my @contentInfo;
my $refContentInfo;

# Initialize default configuration values
$config->{FOREGROUND} 		= 0;
$config->{SHOWVERSION} 		= 0;
$config->{HELP}				= 0;
$config->{DEBUG}			= 0;
$config->{WEBGAOL}			= 0;
$config->{WEBPORT}			= 8080;
#$config->{LOCALIP}			= Sys::HostIP->ip;
$config->{LOCALIP}			= getMyIP();
$config->{WEBURL}			= "";
$config->{URLPREFIX}		= "";
$config->{SERVERNAME}		= VENDOR_ID . " v" . SCRIPT_VERSION;
$config->{MUSICROOT}		= "";
$config->{BUILDINDEX}		= 0;
$config->{SHOWRANDOMS}		= 1;
$config->{PIDFILE}			= DEFAULT_PIDFILENAME;

&$debugFunc("Starting...\n");

do
{
	print "Invalid options specified\n";
	displayUsage();
	exit;
  } unless GetOptions(
	"foreground!"   => \$config->{FOREGROUND},
	"port=i"        => \$config->{WEBPORT},
	"servername=s"  => \$config->{SERVERNAME},
	"urlprefix=s"	=> \$config->{URLPREFIX},
	"webgaol!"		=> \$config->{WEBGAOL},
	"buildindex!"	=> \$config->{BUILDINDEX},
	"xit!"			=> \$config->{XITAFTERBUILD},
	"indexfile:s"	=> \$config->{INDEXFILE},
	"Pidfile:s"		=> \$config->{PIDFILE},
	"Version!"      => \$config->{SHOWVERSION},
	"debug!"		=> \$config->{DEBUG},
	"Help!"			=> \$config->{HELP}
  );

# If they requested the version, print it and exit.
do { print "$0 version " . version() . "\n"; exit; } if $config->{SHOWVERSION};

# If they want help, print it and exit
do { displayUsage(); exit; } if $config->{HELP};

# See that we actually have a directory for the MP3s specified
do
{
	print "No MP3 root directory provided\n";
	displayUsage();
	exit;
} unless $config->{MUSICROOT} = shift;

# Check to see the directory actually exist
die "Specified MP3 root directory $config->{MUSICROOT} does not exist\n" 
	unless -d $config->{MUSICROOT};

# Make sure the path ends with a slash
$config->{MUSICROOT} .= "/" unless substr($config->{MUSICROOT}, -1) eq "/";

# Set up the debug function
$debugFunc = sub { print @_ } if $config->{DEBUG};

# Make sure we don't have a negative port number provided
$config->{WEBPORT} = abs($config->{WEBPORT});

# Build the url string
$config->{WEBURL} = "http://" . $config->{LOCALIP} . ":" . $config->{WEBPORT};

# Go to background unless overridden by options
&$debugFunc("Daemonizing unless forground operation was requested\n");
$config->{FOREGROUND} = 1 if $config->{XITAFTERBUILD};
become_daemon($config) unless $config->{FOREGROUND};

# Get the song data
$refContentInfo = processIndex($config);

# If build and exit was specified (-x) quit now
exit if $config->{XITAFTERBUILD};

# Start the music file web server
if ($config->{URLPREFIX} ne "")
{
	$config->{URLPREFIX} .= "/" unless substr($config->{URLPREFIX}, -1) eq "/";
	&$debugFunc("Songs will be served externally at $config->{URLPREFIX}\n");
}
else
{
	&$debugFunc("Starting the internal web server\n");
	runWebServer($config->{WEBPORT}, $config->{MUSICROOT}, $config->{WEBGAOL});
}

waitForClients($config, $refContentInfo);
#waitForClients($config, \@contentInfo);

# ---------------------------------------------------------------------------- #
#
# Subroutines
#
# ---------------------------------------------------------------------------- #

sub processIndex
{
	my $config = shift;
	my $build = $config->{BUILDINDEX};
	my $write = 0;
	my $read = 0;
	my $refContent;
	my @content;

	$build = 1 unless defined $config->{INDEXFILE};
	$build = 1 if $config->{XITAFTERBUILD};

	# Process the index file
	if (defined $config->{INDEXFILE})
	{
		# Set the file name to the default if the user didn't provide one
		$config->{INDEXFILE} = DEFAULT_INDEXFILENAME 
			if $config->{INDEXFILE} eq '';
			
		&$debugFunc("Index file at $config->{INDEXFILE} specified\n");
	
		# See if the index file exists
		if (-e $config->{INDEXFILE} && $build == 0)
		{
			&$debugFunc("Index file will be read\n");
			$read = 1;
		}
		else
		{
			&$debugFunc("Index file will be created\n");
			$build = 1;
			$write = 1;
		}
	}
	
	if ($read == 1)
	{
		# Read the index
		&$debugFunc("Reading index file\n");
		$refContent = retrieve($config->{INDEXFILE});
		
		unless (defined $refContent)
		{
			&$debugFunc("Index file retrieval failed - will recreate\n");
			$build = 1;
		}
		elsif (@{$refContent}[0]->{VERSION} ne version())
		{
			&$debugFunc("Index file version mismatch - will recreate\n");
			$build = 1;
		}
		else
		{
			&$debugFunc("Index created at $refContent->[0]->{CREATED}\n");
			&$debugFunc("Program version = $refContent->[0]->{VERSION}\n");
		}
		
	}
		
	if ($build == 1)
	{
		&$debugFunc("Indexing songs from $config->{MUSICROOT}\n");
		@content = indexData($config, 
							 findSongs($config), 
							 findPlaylists($config));
		$refContent = \@content;
	}
	
	if ($write == 1)
	{
		&$debugFunc("Writing new index\n");
		store $refContent, $config->{INDEXFILE};
	}
	
	return $refContent;
}

sub waitForClients
# ---------------------------------------------------------------------------- #
# waitForClients:
#
#	Wait for Streamium clients to connect
#
#	parm01:		Configuration data
#	parm02:		Content array
#
#	returns:	Nothing
# ---------------------------------------------------------------------------- #
{
	my $config = shift;
	my $content = shift;
	my $sockSelector;
	my $udpServer;
	my $tcpServer;
	my $readySock;
	my @ready;
	my $udpBroadcastPort = 42591;
	my $listnerPort = 42951;
	
	# Create a udp socket to wait for broadcasts
	&$debugFunc("Creating UPD socket\n");
	$udpServer = new IO::Socket::INET (
		LocalPort => $udpBroadcastPort,
		Proto     => 'udp'
	) or die "Could not create UDP broadcast monitor socket: $!";

	# Create a TCP listner for Streamium requests
	&$debugFunc("Creating TCP socket\n");
	$tcpServer = new IO::Socket::INET (
		LocalPort => $listnerPort,
		Proto => 'tcp',
		Listen => 5,
		Reuse => 1,
	) or die "Could not create socket to listen for tcp requests: $!\n";
	
	# Add both sockets to the selector
	$sockSelector = IO::Select->new($udpServer, $tcpServer);
	
	# Do forever
	while (1)
	{
		&$debugFunc("waiting...\n");
		
		# Wait for a socket to become ready to read
		@ready = $sockSelector->can_read(300);
		&$debugFunc("Socket ready signaled\n") unless scalar @ready == 0;		
		
		# Loop through all sockets that are ready to read
		foreach $readySock (@ready)
		{
			if ($readySock == $udpServer)
			{
				# Client has broadcasted looking for me
				my $datagram;
				
				# Receive packet and call subroutine to process
				&$debugFunc("Receiving data on upd socket\n");
				$readySock->recv($datagram, 4096);
				helloResp($config, $datagram, $listnerPort);
				
			}
			elsif ($readySock == $tcpServer)
			{
				# Listener has pinged - accept the connection
				&$debugFunc("Accepting socket\n");
				$sockSelector->add($readySock->accept());
			}
			else
			{
				# Read from client socket
				&$debugFunc("Client socket signaled read ready\n");
				
				# Kill connection if subroutine requests it
				unless (reqResp($readySock, $content, $config))
				{
					$sockSelector->remove($readySock);
					close $readySock;
				}
			}
		}
	}
}

sub reqResp
# ---------------------------------------------------------------------------- #
# reqResp:
#
#	Responds to the Streamium client. The Streamium client is expected to send 
#	an XML string that describes the set of directories or playable items that
#	it wishes to display to the user. This subroutine will extract the request
#	data, build a response and send to the Streamium.
#
#	parm01:		The socket from which to receive the request
#	parm02:		Content array
#	parm03:		Configuration data
#
#	returns:	0 - The socket was closed by the client
#				1 - The data was received and processed.
# ---------------------------------------------------------------------------- #
{
	my $reqSock = shift;
	my $content = shift;
	my $config = shift;
	my $buf = "";
	my $parser;                                           
	my $doc;
	my $node;
	my $pNode;
	my $parentNode;
	my $numNode;
	my $offsetNode;
	my $superScroll;
	my $clientId;
	my $header;
	my $data;
	my $dataLen;
	my $respGenerator;
	my %generators = 
	(
		'makerandoms', \&makeRandoms
	);
	
	use constant PREAMBLE => "POST // HTTP/1.0";

	# Receive the data
	&$debugFunc("Receiving data on client socket\n");
	$reqSock->recv($buf, 4096);
	
	# If there is no data assume the client closed the connection
	if (!defined $buf || length($buf) == 0)
	{
		&$debugFunc("Client closed connection\n");
		
		# Tell caller socket has gone
		return 0;
	}
	
	&$debugFunc("Received request\n");
	&$debugFunc("$buf\n");

	# Ensure data actually looks like a Streamium request string
	unless (substr($buf, 0, length PREAMBLE) eq PREAMBLE)
	{
		&$debugFunc("Unrecognized request received from client\n");
		
		# Dump sockets that send garbage
		return 0;
	}

	&$debugFunc("Appears to be a Stream navigation data request\n");

	# Extract relavent XML nodes
	$parentNode = ($buf =~ /<nodeid>(.*)<\/nodeid>/ ? $1 : 0);
	$numNode = ($buf =~ /<numelem>(.*)<\/numelem>/ ? $1 : 0);
	$offsetNode = ($buf =~ /<fromindex>(.*)<\/fromindex>/ ? $1 : 0);
	$superScroll = ($buf =~ /<superscroll>(.*)<\/superscroll>/ ? $1 : "");
	
	# This is a bit iffy but I suspect the nc12 tag uniquely identifies the
	# Streamium. If you have more than one your results may vary
	$clientId = ($buf =~ /<nc12>(.*)<\/nc12>/ ? $1 : "Unknown");
	
	$pNode = @{$content}[$parentNode];
	
	if (exists $pNode->{GENERATOR})
	{
		&$debugFunc("Generator nodes with $pNode->{GENERATOR}\n");
		$data = &{$generators{$pNode->{GENERATOR}}}(
			$content, 
			$pNode, 
			$offsetNode, 
			$superScroll, 
			$numNode,
			$clientId,
			$config);
	}
	
	else
	{
		&$debugFunc("Return nodes at $parentNode from ",
			"$offsetNode for $numNode\n");
		
		$data = createResponse($content, 
			$pNode, 
			$offsetNode, 
			$superScroll, 
			$numNode,
			$clientId,
			$config);
	}
	
	$data = makeNode('contentdataset', $data);
	$dataLen = length $data;
	$header = "HTTP/1.0 200 OK\r\n" .
			  "Accept-Ranges: bytes\r\n" .
			  "Content-Length:$dataLen\r\n" .
			  "Content-Type: text/xml\r\n" . 
			  "\r\n";
#	$data = encode("iso-8859-1", $data);
	&$debugFunc("Response:\n$header$data\n:end\n");
	$reqSock->send($header . $data);

	return 1;
}

sub createResponse
# ---------------------------------------------------------------------------- #
# createResponse: 
#
#	Take the request from the Streamium and turn it into an XML response.
#
#	parm01:		Reference to the content array
#	parm02:		Parent of nodes to return
#	parm03:		Offset into the child nodes to return from
#	parm04:		Superscroll letter (skip to letter entered on Streamium)
#	parm05:		Maximum number of nodes to return
#	parm06:		Unique Streamium device identifier (maybe)
#	parm07:		Configuration data
#
#	returns:	XML string with nodes' information
# ---------------------------------------------------------------------------- #
{
	my $content = shift;
	my $parentNode = shift;
	my $offsetNode = shift;
	my $superScroll = shift;
	my $numNode = shift;
	my $clientId = shift;
	my $config = shift;
	my $countNode = 0;
	my $childNodes;
	my $nodeIt;
	my $nodeIndex;
	my $node;
	my $lastNode;
	my $response = '';
	my $url;
	
	if ($superScroll ne "")
	{
		# Calculate offset node by looking for child greater than or equal to
		# the letter passed in by superscroll
	
		$superScroll = lc($superScroll);
		
		for ($nodeIt = 0; $nodeIt < $parentNode->{NODECOUNT}; ++$nodeIt)
		{
			$nodeIndex = @{$parentNode->{CHILDREN}}[$nodeIt];
			$node = @{$content}[$nodeIndex];
			
			last if (lc(substr($node->{NAME}, 0, 1)) ge $superScroll);
		}
		
		$offsetNode = ($nodeIt >= $parentNode->{NODECOUNT})?
			$parentNode->{NODECOUNT} : $nodeIt;
	}
	
	# Build the url prefix for local serving or external web server.
	$url = ($config->{URLPREFIX} ne "")? 
		$config->{URLPREFIX} : $config->{WEBURL} . $config->{MUSICROOT};

	for ($nodeIt = $offsetNode; $nodeIt < $parentNode->{NODECOUNT}; ++$nodeIt)
	{
		$nodeIndex = @{$parentNode->{CHILDREN}}[$nodeIt];
		$node = @{$content}[$nodeIndex];

		if ($node->{'TYPE'} eq 'SONG')
		{
			$response .= makeNode('contentdata',
				makeNode('name', $node->{'NAME'}) .
				makeNode('nodeid', $nodeIndex) .
				makeNode('playable') .
				makeNode('url', $url . 
					substr($node->{'FILENAME'}, length($config->{MUSICROOT}))) .
				makeNode('title', $node->{'NAME'}) .
				makeNode('album', $node->{'ALBUM'}) .
				makeNode('trackno', $node->{'TRACK'}) .
				makeNode('artist', $node->{'ARTIST'}) .
				makeNode('genre', $node->{'GENRE'}) .
				makeNode('year', $node->{'YEAR'}) .
				makeNode('playlength', $node->{'TIME'}));
		}
		elsif ($node->{TYPE} eq 'PLAYLISTENTRY')
		{
			$response .= makeNode('contentdata',
				makeNode('name', $node->{'NAME'}) .
				makeNode('nodeid', $nodeIndex) .
				makeNode('playable') .
				makeNode('url', $node->{URL}) . 
				makeNode('title', $node->{'NAME'}) .
				makeNode('playlength', $node->{'TIME'}));
		}
		else
		{
			$response .= makeNode('contentdata',
				makeNode('name', $node->{'NAME'}) .
				makeNode('nodeid', $nodeIndex) .
				makeNode('branch'));
		}
		last if ++$countNode == $numNode; 
	}

	$response .= makeNode('totnumelem', $parentNode->{NODECOUNT});
	$response .= makeNode('fromindex', $offsetNode);
	$response .= makeNode('numelem', $countNode);
	$response .= makeNode('alphanumeric');
	
	return $response;
}

sub makeRandoms
# ---------------------------------------------------------------------------- #
# makeRandoms:
#
#	This is a generator function. It will generate content for the client on the
#	fly. It will save its state for use by the same client later. This function
#	will generate a random collection of songs or albums (in track order) for 
#	the	client.
#
#	parm01:		Reference to the content array
#	parm02:		Parent of nodes to return
#	parm03:		Offset into the child nodes to return from
#	parm04:		Superscroll letter (skip to letter entered on Streamium)
#	parm05:		Maximum number of nodes to return
#	parm06:		Unique Streamium device identifier (maybe)
#	parm07:		Configuration data
#
#	returns:	XML string with nodes' information
# ---------------------------------------------------------------------------- #
{
	my $content = shift;
	my $parentNode = shift;
	my $offsetNode = shift;
	my $superScroll = shift;
	my $numNode = shift;
	my $clientId = shift;
	my $config = shift;
	my ($randomType, $randomCount) = @{$parentNode->{PARMS}};
	my $superRoot;
	my $rootIndex;
	my $root;
	my $nodeIndex;
	my $nodeEnd;
	my $node;
	my $maxRand;
	my %uniqueRandoms;
	my $randomEntry;
	my $url;
	my $response;
	my $highNode = scalar @{$content};

	$superRoot = $content->[0];

	&$debugFunc("Retrieving random list\n");

	if ($offsetNode == 0)
	{
		&$debugFunc("Generating $randomCount $randomType\n");
		
		# Generate a new random list
		foreach $rootIndex (@{$superRoot->{CHILDREN}})
		{
			if (uc($content->[$rootIndex]->{NAME}) eq $randomType)
			{
				$root = $content->[$rootIndex];
				last;
			}
		}
		
		$maxRand = scalar @{$root->{CHILDREN}};
		$randomCount = $maxRand - 1 if $randomCount >= $maxRand;
		
		while ($randomCount > 0)
		{
			do
			{
				$randomEntry = int(rand($maxRand));
			} while exists $uniqueRandoms{$randomEntry};
			
			$node = $content->[$root->{CHILDREN}->[$randomEntry]];
			$uniqueRandoms{$randomEntry} = $node;
			$randomCount--;
		}
		
		delete $superRoot->{CLIENTS}->{$clientId};
		
		if ($randomType eq 'SONGS')
		{
			@{$superRoot->{CLIENTS}->{$clientId}} = values %uniqueRandoms;
		}
		else
		{
			foreach $rootIndex (values %uniqueRandoms)
			{
				foreach $nodeIndex (@{$rootIndex->{CHILDREN}})
				{
					$node = $content->[$nodeIndex];
					push @{$superRoot->{CLIENTS}->{$clientId}}, $node;
				}
			}
		}
	}

	# Build the url prefix for local serving or external web server.
	$url = ($config->{URLPREFIX} ne "")? 
		$config->{URLPREFIX} : $config->{WEBURL} . $config->{MUSICROOT};
		
	$nodeIndex = $offsetNode;
	$nodeEnd = $offsetNode + $numNode;
	
	$nodeEnd = @{$superRoot->{CLIENTS}->{$clientId}}
		if $nodeEnd > @{$superRoot->{CLIENTS}->{$clientId}};
		
	$nodeEnd--;

	foreach $nodeIndex ($nodeIndex .. $nodeEnd)
	{
		$node = $superRoot->{CLIENTS}->{$clientId}->[$nodeIndex];
		$response .= makeNode('contentdata',
			makeNode('name', $node->{'NAME'}) .
			makeNode('nodeid', $nodeIndex + $highNode) .
			makeNode('playable') .
			makeNode('url', $url . 
				substr($node->{'FILENAME'}, length($config->{MUSICROOT}))) .
			makeNode('title', $node->{'NAME'}) .
			makeNode('album', $node->{'ALBUM'}) .
			makeNode('trackno', $node->{'TRACK'}) .
			makeNode('artist', $node->{'ARTIST'}) .
			makeNode('genre', $node->{'GENRE'}) .
			makeNode('year', $node->{'YEAR'}) .
			makeNode('playlength', $node->{'TIME'}));
	}

	$response .= makeNode('totnumelem', 
		scalar @{$superRoot->{CLIENTS}->{$clientId}});
	$response .= makeNode('fromindex', $offsetNode);
	$response .= makeNode('numelem', $nodeEnd - $nodeIndex + 1);
	$response .= makeNode('alphanumeric');
	
	return $response;
}

sub helloResp
# ---------------------------------------------------------------------------- #
# helloResp:
#
#	Responds to a UDP broadcast message from a client
#
#	parm01:		datagram received from client
#	parm02:		IP address of machine we are running on
#	parm03		TCP/IP port we listen for requests upon
#
#	returns:	Nothing
# ---------------------------------------------------------------------------- #
{
	my $config = shift;
	my $datagram = shift;
	my $listnerPort = shift;
	my $helloPid;
	my $clientPort;
	my $clientIP;
	my $helloSock;
	my $parser;                                           
	my $doc;
	my $node;
	my $serverResp;
	
	# If the packet isn't ours ignore it
	return unless $datagram =~ /^<PCLinkClient>/;

	&$debugFunc("Appears to be a Streamium client\n");
	
	$clientIP = ipConvert(($datagram =~ /<IP>(.*)<\/IP>/ ? $1 : 0));
	$clientPort = portConvert(($datagram =~ /<Port>(.*)<\/Port>/ ? $1 : 0));

	# Fork a new process to prevent main process being blocked by our attempt to
	# connect to the client
	$helloPid = fork();
	
	# Return if this is the main process
	return if $helloPid;
	
	# Die if fork failed
	die "helloResp fork failed - $!\n" unless defined $helloPid;
	
	&$debugFunc("Returning response to $clientIP : $clientPort\n");
	
	# Open tcpsock connection
	$helloSock = new IO::Socket::INET(
		PeerAddr => $clientIP,
		PeerPort => $clientPort,
		Proto    => 'tcp'
	) or die "Socket could not be created: $!\n";

	# Build message for client and send it
	$serverResp = makeNode( "PCLinkServer",
		    makeNode( "Version",   SCRIPT_VERSION )
		  . makeNode( "VendorID",  VENDOR_ID )
		  . makeNode( "name",      $config->{SERVERNAME} )
		  . makeNode( "ShortName", $config->{SERVERNAME} )
		  . makeNode( "IP",        ipPack($config->{LOCALIP}))
		  . makeNode( "Port",      portConvert($listnerPort)));

	&$debugFunc("Return message to client's TCP/IP server port\n");
	$helloSock->send($serverResp);

	# Close the response socket
	close($helloSock);
	
	# End the substrprocess
	exit();
}

sub runWebServer
# ---------------------------------------------------------------------------- #
# runWebServer: 
#
#	Implements a cut down web server in a child process.
#
#	parm01:		web server port
#	parm02:		MP3 directory root
#	parm03:		Web gaol. 1=only serve from directory root tree
#
#	returns:	Nothing
# ---------------------------------------------------------------------------- #
{
	my ( $mp3Port, $musicRoot, $webGaol ) = @_;
	my $server;
	my $client;
	my $httpdPid;
	my $serverPid;
	my $reqFile;
	my $f;
	my $size;
	my $dataLen;
	my @ranges;
	my @byteRanges;
	my @oneRange;
	my $i;
	my $buf;
	my $bufLen;
	my $selector;
	my $selRead;
	my $selWrite;
	my $selError;
	my $hFile;
	use constant BUFSIZE => 4096 * 4;

	&$debugFunc("Forking web server process\n");
	$serverPid = fork();
	
	# Die if process could not be started
	die "Can't start web server process" if ( not defined $serverPid );

	if ($serverPid)
	{
		# Parent: Write child pid out and exit 
		&$debugFunc("Web server started on pid $serverPid\n");
		return $serverPid;
	}

	# Child: Start web server
	&$debugFunc("Creating web server listner on port $mp3Port\n");
	$server = IO::Socket::INET->new(
		Proto     => 'tcp',
		LocalPort => $mp3Port,
		Listen    => SOMAXCONN,
		Reuse     => 1
	) || die "Server setup failed - $!";
	
	&$debugFunc("Web server waiting on port $mp3Port\n");

	while ( $client = $server->accept() )
	{
		# Serve Multiple Clients at the same time by forking new processes for
		# each request.

		&$debugFunc("Forking slave to serve song...\n");
		$httpdPid = fork();

		# Parent loops
		do { 
			&$debugFunc("Forked process $httpdPid\n");
			&$debugFunc("Waiting for clients...\n"); 
			$client->close; 
			next; 
		} if $httpdPid;

		# Serve up music file in child

		eval
		{
			# Get the http request from the streamium
			my $request = <$client>;
			&$debugFunc("------------ REQUEST ------------\n");
			&$debugFunc("$request\n");

			if ( $request =~ m|^GET (.+) HTTP/1.[01]| )
			{
				$reqFile = unescape($1);
				
				if ($webGaol)
				{
					# Kill files requests with parent directory dot dots in
					die "Invalid request\n" if $reqFile =~ /\.\.\//;
					
					# Kill paths that are too short
					die "Invalid request\n" 
						if length($reqFile) <= length($musicRoot);
						
					# Tear off front and replace with my root
					# This should make no difference if request is kocher
					$reqFile = substr($reqFile, length($musicRoot));
					$reqFile = $musicRoot . $reqFile;
					
				}

				&$debugFunc("Returning $reqFile\n");

				if ( -e $reqFile )
				{
# Not working properly so commented out for now					
#					updateHistory( $musicRoot, $reqFile, 
#						inet_ntoa( $client->peeraddr ) );

					open( $f, "<$reqFile" )
					  || die "Couldn't open file: $reqFile\n";

					($size) = ( stat($f) )[7];
					
					die "Requested file is zero bytes long\n" unless $size > 0;
					$dataLen = 0;
					&$debugFunc("------------ HEADERS ------------\n");

					while (<$client>)
					{
						last if $_ eq "\r\n";
						s/\s//g;
						print $_, "\n";

						# Streamium may send a range request. If we find one 
						# parse it out and maintain an array that lists the 
						# different portions of the file to return.

						if (/range:bytes=(.*)/i)
						{
							my @byteRanges = split( /,/, $1 );

							foreach (@byteRanges)
							{
								@oneRange = split(/-/);
								
								die "Range parsing error" unless 
									@oneRange <= 2 && @oneRange > 0;

								if ( $oneRange[0] eq '' )
								{
									$oneRange[0] = 
										max( 0, $size - $oneRange[1] );
									$oneRange[1] = $size - 1;
								}
								elsif ( !defined( $oneRange[1] ) )
								{
									$oneRange[0] = min($size - 1, $oneRange[0]);
									$oneRange[1] = $size - 1;
								}
								else
								{
									$oneRange[0] =
									  min($size - 1, $oneRange[0]);
									$oneRange[1] =
									  min($size - 1, $oneRange[1]);
								}

								$dataLen += $oneRange[1] - $oneRange[0] + 1;
								push @ranges, [@oneRange];
							}
						}
					}

					# Set up range for the whole file if none were specified

					if ( !@ranges )
					{
						push @ranges, [ ( 0, $size - 1 ) ];
						$dataLen = $size;
					}

					# Send the file
					&$debugFunc("Send Music file\n");

					$selector = new IO::Select($client);
					
					&$debugFunc("Wait for client to be ready for write\n");
					($selRead, $selWrite, $selError)
						= IO::Select::select($selector, $selector, $selector);
						
					if (scalar @{$selWrite})
					{
					}
					elsif (scalar @{$selRead} || scalar @{$selError})
					{
						&$debugFunc("Read = ", scalar @{$selRead}, "\n");
						&$debugFunc("Write = ", @{$selWrite}, "\n");
						&$debugFunc("Error = ", scalar  @{$selError}, "\n");
						goto EmergencyExit;
					}

					&$debugFunc("Write headers\n");
					$client->send("HTTP/1.1 200 OK\r\n");
					$client->send("Content-Length: $dataLen\r\n");
					$client->send("Connection: close\r\n");
					$client->send("Content-Type: audio/mpeg\r\n");
					$client->send("\r\n");
					binmode $f, ":raw";

					&$debugFunc("Write data\n");
					foreach (@ranges)
					{
						&$debugFunc("Send range ", $_->[0], " ", $_->[1], "\n");
						seek( $f, $_->[0], SEEK_SET )
						  || die("Failed to seek to start position");

						$dataLen = $_->[1] - $_->[0] + 1;

						while ( $dataLen > 0 )
						{
							$bufLen = min($dataLen, BUFSIZE);
							read( $f, $buf, $bufLen )
							  || die("Failed to read MP3 file");

							($selRead, $selWrite, $selError)
								= IO::Select::select($selector, 
													 $selector, 
													 $selector);
								
							if (scalar @{$selWrite})
							{
							}
							elsif (scalar @{$selRead} || scalar @{$selError})
							{
								&$debugFunc("Read = ", 
											scalar @{$selRead}, "\n");
								&$debugFunc("Write = ", 
											@{$selWrite}, "\n");
								&$debugFunc("Error = ", 
											scalar  @{$selError}, "\n");
								goto EmergencyExit;
							}

							$client->send($buf)
							  || die("Unable to write to client socket");

							$dataLen -= $bufLen;
						}
					}
				}
				else
				{
					&$debugFunc("File not found\n");
					sendErrorPage( $client, 404, "FILE NOT FOUND" );
				}
			}
			else
			{
				&$debugFunc("Bad request\n");
				sendErrorPage( $client, 400, "BAD REQUEST" );
			}
		};

		if ( $@ )
		{
			sendErrorPage( $client, 500, "INTERNAL SERVER ERROR" );
			&$debugFunc("Error: " . $@);
		}

		goto SubExit;
		
EmergencyExit:		
		&$debugFunc("Emergency exit used to quit\n");
		
SubExit:

		&$debugFunc("Slave song server terminating\n");

		eval {
			close($f);
		};
		
		close($client);
		exit;    # child exits
	}

	&$debugFunc("Web server exited unexpectedly\n");
	exit;
}

sub min
# ---------------------------------------------------------------------------- #
# min:
#
#	Returns the smallest value of two numeric parameters. If they equal the 
#	first will be returned.
#
#	parm01:		First number
#	parm02:		Second number
#
#	returns:	Smallest number
# ---------------------------------------------------------------------------- #
{
	return ($_[0] <= $_[1])? $_[0] : $_[1];
}

sub unescape
# ---------------------------------------------------------------------------- #
# unescape:
#
#	Decodes escape characters in a URL string
#
#	parm01:		URL to deocde
#
#	returns:	Decoded URL
# ---------------------------------------------------------------------------- #
{
    my $toDecode = shift;
    $toDecode =~ tr/+/ /;    # pluses become spaces
    $toDecode =~ s/%([0-9a-fA-F]{2})/pack("c",hex($1))/ge;
    return $toDecode;
}

sub sendErrorPage
# ---------------------------------------------------------------------------- #
# sendErrorPage:
#
#	Sends an error page back to the http client.
#
#	parm01:		client socket
#	parm02:		error number
#	parm03:		message
#
#	returns:	Nothing
# ---------------------------------------------------------------------------- #
{
	my ( $client, $errNum, $errMsg ) = @_;

	print $client "HTTP/1.0 $errNum, $errMsg\r\n";
	print $client "Content-Type: text/plain\r\n\r\n";
	print $client "$errNum: $errMsg\n";
}

sub updateHistory
# ---------------------------------------------------------------------------- #
# updateHistory:
#
#	Adds the requested file to the history M3U file. This allows the Streamium
#	user to look at and replay files that have been played recently.
#
#	parm01:		Directory for M3U file - typically the MP3 root directory
#	parm02:		File name
#	parm03:		Client's IP address
#
#	returns:	Nothing
# ---------------------------------------------------------------------------- #
{
	# TODO - This will need to be rewritten - we already have the tags
	my ( $musicRoot, $reqFile, $clientIP ) = @_;
	my $tagInfo      = extractTags($reqFile);
	my $m3uFile      = "$musicRoot/##hist_$clientIP.m3u";
	my $m3uFileNew   = $m3uFile . ".new";
	my $seconds = 0;
	my $newLine;
	my $oldLine;
	my $i;
	
	local $_;
	local $@;

	&$debugFunc("updateHistory $musicRoot, $reqFile, $clientIP\n");

	&$debugFunc("Artist=" . $tagInfo->{ARTIST} . "\n" .
		       "Album=" . $tagInfo->{ALBUM} . "\n" .
		       "Title=" . $tagInfo->{TITLE} . "\n" .
			   "Seconds=" . $tagInfo->{TIME} . "\n");
	
	# Only update the history if the file has ID3V2 tags present
	return if (!$tagInfo->{TITLE} ||
			   !$tagInfo->{ARTIST} ||
			   !$tagInfo->{ALBUM});

	$tagInfo->{TIME} = -1 unless defined $tagInfo->{TIME};
	
	eval 
	{
		&$debugFunc("Open\n");
		open( NEWM3U, ">$m3uFileNew" )
		  	|| die "Unable to create new M3U file";

		&$debugFunc("Print header\n");
		print NEWM3U "#EXTM3U\r\n";
		$newLine =
		  	"#EXTINF:" . $tagInfo->{TIME} . "," . 
			$tagInfo->{ARTIST} . " - " . 
			$tagInfo->{TITLE} . "\n";
		&$debugFunc("Print $newLine\n");
		print NEWM3U $newLine;
		&$debugFunc("Print $reqFile\n");
		print NEWM3U $reqFile, "\n";

		if ( -e $m3uFile )
		{
			&$debugFunc("Open old\n");

			open( OLDM3U, "<$m3uFile" )
			  	|| die "Unable to open old M3U file";

			# Read and lose original header
			$oldLine = <OLDM3U>;
			$i = 0;

			&$debugFunc("Loop old\n");

			while ( $oldLine = <OLDM3U> )
			{
				last unless $i++ < 39; 

				&$debugFunc("Read $oldLine\n");

				if ( $oldLine eq $newLine )
				{
					# Same song as the new one - lose the filename line
					&$debugFunc("Duplicate entry dropped\n");
					$oldLine = <OLDM3U>;
				}
				else
				{
					# Add new line to file
					&$debugFunc("Copy old lines\n");
					print NEWM3U $oldLine;
					print NEWM3U <OLDM3U>;
				}
			}
					
			&$debugFunc("Close old\n");

			close OLDM3U || die "Failed to close old M3U file";

			&$debugFunc("Delete old\n");
			unlink "$m3uFile"
			  || die "Unable to delete old M3U file";
		}

		&$debugFunc("Close new\n");
		close NEWM3U || die "Failed to close new M3U file";

		&$debugFunc("Rename new\n");
		rename "$m3uFileNew", "$m3uFile"
			|| die "Rename of new M3U file to original name failed";
	};

	if ($@)
	{
		close NEWM3U;
		close OLDM3U;
		&$debugFunc(warn "Error:  $@ \n");
		warn "Error: " . $@;
	}
}

sub getMyIP
# ---------------------------------------------------------------------------- #
# getMyIP:
#
#	Return the IP address of this device.
#
#	returns:	IP Address
# ---------------------------------------------------------------------------- #
{
	my @addr = inet_aton(hostname_long()) or die "Unable to retrieve device name";
	my ($ip0, $ip1, $ip2, $ip3) = unpack('C4', $addr[0]);
	
	return "$ip0.$ip1.$ip2.$ip3";
}

sub portConvert
# ---------------------------------------------------------------------------- #
# portConvert:
#
#	Converts a little endian decimal string into the perl equivelent number.
#	('51111' -> 0xC7A7 -> 0xA7C7 -> 42951)
#	parm01:		Value to convert
#
#	returns:	Converted value
# ---------------------------------------------------------------------------- #
{
	my $portNo = shift;
	
	$portNo = substr(unpack("H*", pack("N", $portNo)), 4);
	$portNo =~ s/(..)(..)/$2$1/;

	return hex($portNo);
}

sub ipConvert
# ---------------------------------------------------------------------------- #
# ipConvert:
#
#	Converts a little endian decimal IP address into a traditional format. For
#	example: 1694607552 becomes '192.168.1.101'.
#
#	parm01:		Value to convert
#
#	returns:	Converted value
# ---------------------------------------------------------------------------- #
{
    my $ipNo = shift;
    
    my $work = unpack("H*", pack("N", $ipNo));
    $work =~ s/(..)(..)(..)(..)/$4X$3X$2X$1/;
    my @t = split(/X/, $work);
    return hex($t[0]) . "." . hex($t[1]) . "." . hex($t[2]) . "." . hex($t[3]);
}

sub ipPack
# ---------------------------------------------------------------------------- #
# ipPack:
#
#	This is the antonym of ipConvert above.
#
#	parm01:		Value to convert
#
#	returns:	Converted value
# ---------------------------------------------------------------------------- #
{
	my $ip = shift;
	
	my @t = split(/\./, $ip);
	my $ret = $t[0] + ( $t[1] << 8 ) + ( $t[2] << 16 ) + ( $t[3] << 24 );
	
	return $ret;
}

sub makeNode
# ---------------------------------------------------------------------------- #
# makeNode:
#
#	Converts the parameters into an XML node. If no value is specified it will
#	return a short node eg <mynode/>.
#
#	parm01:		node name
#	parm02:		node value
#
#	returns:	A string containing the XML node
# ---------------------------------------------------------------------------- #
{
	my ( $name, $value ) = @_;

	if (defined $value && substr($value, 0, 1) ne "<")
	{
		$value = XMLEscape($value);
	}
	
	return (defined $value) 
		? "<$name>"	. $value . "</$name>" : "<$name/>";
}

sub XMLEscape
# ---------------------------------------------------------------------------- #
# XMLEscape:
#
#	Ensures that any special characters are escaped in the resulting XML value.
#
#	parm01:		XML node value
#
#	returns:	Node value with special characters escaped.
#
# Notes:
#	Ampersand		& 	&amp;
#	Greater-than	> 	&gt;
#	Less-than 		< 	&lt;
#	Apostrophe 		' 	&apos;
#	Quote 			" 	&quot; 
# ---------------------------------------------------------------------------- #
{
	my $value = shift;

	$value =~ s/&/&amp;/g;
	$value =~ s/>/&gt;/g;
	$value =~ s/</&lt;/g;
	$value =~ s/'/&apos;/g;
	$value =~ s/"/&quot;/g;

	return encode('UTF8', $value);
}

sub become_daemon
# ---------------------------------------------------------------------------- #
# become_daemon:
#
#	Forks a background task and ends the foreground task freeing the caller.
#
#	returns:	Process ID
# ---------------------------------------------------------------------------- #
{
	my $config = shift;
	my $hFile;
	
	die "Can't fork" unless defined( my $child = fork );
	
	if ($child)
	{
		# Parent: Write child pid out and exit 
		&$debugFunc("Child started on pid $child\n");
		&$debugFunc("Writing pid to file $config->{PIDFILE}\n");
		open($hFile, '>', $config->{PIDFILE}) 
			or die "Unable to open $config->{PIDFILE}";
		print $hFile $child, "\n" or die "Unable to write $config->{PIDFILE}";
		close $hFile or die "Unable to close $config->{PIDFILE}";
		exit 0;
	}
		
	&$debugFunc("Child daemon is up and running\n");

	setsid();            # become session leader
	open( STDIN,  "</dev/null" );
	open( STDOUT, ">/dev/null" ) unless $config->{DEBUG};
	open( STDERR, ">&STDOUT" );
	chdir '/';           # change working directory
	umask(0);            # forget file mode creation mask
	$ENV{PATH} = '/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin';
	return $$;
}

sub indexData
# ---------------------------------------------------------------------------- #
# indexData:
#
#	Takes the array of song hashes and creates an indexed array from them 
#	building a parent child relationship between different objects. The array
#	is then returned to the caller. See code comments for information on the
#	relationships created. 

#	The returned array is ready to create XML to send to the Streamium.
#
#	parm01:		Configuration data
#	parm02:		Reference to an array of song hashes
#	parm02:		Reference to an array of playlist hashes
#
#	returns:	Array of root selections, artists, albums, genres, playlists, 
#				playlist entries and songs
# ---------------------------------------------------------------------------- #
{
	use constant INDEX_ROOT			=>		0;

	my $config = shift;
	my $songs = shift;
	my $playlists = shift;
	my $song;
	my $songIndex;
	my @artists;
	my @artistsAllSongs;
	my @albums;
	my @genres;
	my %genresWork;
	my $artist = {};
	my $artistAllSongs = {};
	my $album = {};
	my $genre = {};
	my $lastArtist = '';	# Keep track of current artist name
	my $lastAlbum = '';		# Keep track of current album name
	my @indexed; # = ({}, {}, {}, {}, {}, {});
	my @randoms;
	my $entry;
	my $artistsStart;
	my $albumsStart;
	my $genresStart;
	my $playlistsStart;
	my $playlistsCount;
	my $randomsStart;
	my $songsStart;
	my $artistsAllSongsStart;
	my $nextRoot;
	my $extras;
	my $i;
	my ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst) = 
		localtime(time);
	my $timestamp = sprintf "%4d-%02d-%02d %02d:%02d:%02d",
		$year+1900, $mon+1, $mday, $hour, $min, $sec;
	
	# No songs found so quit now
	die "Error: No songs found\n" unless scalar @{$songs} > 0;
	
	# Loop through all of the songs in the array.
	# Create a new array for each of artists, albums, genres and all songs
	foreach $songIndex (0.. scalar @{$songs} - 1)
	{
		# Get the song
		$song = $songs->[$songIndex];
		
		# Is this a new artist?
		if (lc($lastArtist) ne lc($song->{ARTIST}))
		{
			# New artist:
			# Create an artist hash and add it to the array
			$artist = {};
			push @artists, $artist;
			
			# Set the artist name and hash type
			$lastArtist = $artist->{NAME} = $song->{ARTIST};
			$artist->{TYPE} = 'ARTIST';
			$artist->{CHECK} = 'DONE';
			
			# Initialize the children array
			@{$artist->{CHILDREN}} = ();
			
			# Create an 'all songs' album add add it to the array
			$artistAllSongs = {};
			push @artistsAllSongs, $artistAllSongs;
			
			# Set the name and type
			$artistAllSongs->{NAME} = 'All Songs';
			$artistAllSongs->{TYPE} = 'ALLSONGS';
			
			# Initialize the children array
			@{$artistAllSongs->{CHILDREN}} = ();
			
			# Add it to the artist's children (albums)
			push( @{$artist->{CHILDREN}}, $#artistsAllSongs );
			
			# Force a new album
			$lastAlbum = "";
			#&$debugFunc("Added new artist: " . $artist->{NAME} . "\n");
		}

		# Is this a new album?
		if (lc($lastAlbum) ne lc($song->{ALBUM}))
		{
			# New album:
			# Create an album hash and add it to the array
			$album = {};
			push @albums, $album;
			
			# Set the album name and hash type
			$lastAlbum = $album->{NAME} = $song->{ALBUM};
			$album->{TYPE} = 'ALBUM';

			# Add this album to the children of the artist
			push( @{$artist->{CHILDREN}}, $#albums );
			
			# Initialize the track array
			@{$album->{CHILDREN}} = ();
			#&$debugFunc("\tAdded new album: " . $album->{NAME} . "\n");
		}
		
		# Add the song to the 'all songs' album and the real album
		push( @{$artistAllSongs->{CHILDREN}}, $songIndex );
		push( @{$album->{CHILDREN}}, $songIndex );
		
		# Is this a new genre?
		if (exists $song->{GENRE} && ! exists $genresWork{$song->{GENRE}})
		{
			# Create a new genre hash
			$genre = {};
			
			# Add the genre to the genres tracking hash
			$genresWork{$song->{GENRE}} = $genre;

			# Add the genre name and hash type
			$genre->{TYPE} = 'GENRE';
			$genre->{NAME} = $song->{GENRE};
			
			# Initialize the song array
			@{$genre->{CHILDREN}} = ();
		}
		
		# Add the song to the genre hash
		push( @{$genre->{CHILDREN}}, $songIndex ) if exists $song->{GENRE};
	}
	
	# Create a sorted array of genres
	@genres = sort {
		return lc($a->{NAME}) cmp lc($b->{NAME});
	} values %genresWork;
	
	# Build the randoms array
	foreach $i (10, 20, 50, 100)
	{
		%{$randoms[scalar @randoms]} = (
			'TYPE' 		=> 		'RANDOM',
			'NAME' 		=> 		"$i Random Songs",
			'GENERATOR' => 		"makerandoms",
			'PARMS' 	=> 		[('SONGS', $i)]);
	}
	
	foreach $i (1, 2, 5, 10)
	{
		%{$randoms[scalar @randoms]} = (
			'TYPE'		=>	 	'RANDOM',
			'NAME'		=>		"$i Random Albums",
			'GENERATOR'	=>		"makerandoms",
			'PARMS'		=>		[('ALBUMS', $i)]);
	}
	
	# Display the statistic of the songs found
	print "Found " . scalar @artists . " artists\n";
	print "Found " . scalar @albums . " albums\n";
	print "Found " . scalar @genres . " genres\n";
	print "Found " . @{$playlists}[0]->{NODECOUNT} . " playlists\n";
	print "Found " . scalar @randoms . " random generators\n";
	print "Found " . scalar @{$songs} . " songs\n";
	
	# Now add the nodes that appear on the Streamium as the initial selections.
	# Each root node contains a children array that points to the specific type
	# that it references. Eg Artists -> list of artists -> list of albums ->
	# list of songs

	# First just a skeleton for each entry
	
	# Supernode
	$indexed[INDEX_ROOT]->{TYPE} = 'SUPER';
	$indexed[INDEX_ROOT]->{VERSION} = version();
	$indexed[INDEX_ROOT]->{CREATED} = $timestamp;
	$nextRoot = INDEX_ROOT + 1;
	
	# Artists
	$indexed[$nextRoot]->{TYPE} = 'ROOT';
	$indexed[$nextRoot]->{'NAME'} = 'Artists';
	$nextRoot++; 
	
	# Albums
	$indexed[$nextRoot]->{TYPE} = 'ROOT';
	$indexed[$nextRoot]->{'NAME'} = 'Albums';
	$nextRoot++; 

	# Genres (if any were found)
	if (scalar @genres > 0)
	{
		$indexed[$nextRoot]->{TYPE} = 'ROOT';
		$indexed[$nextRoot]->{'NAME'} = 'Genres';
		$nextRoot++; 
	}
	
	# Songs
	$indexed[$nextRoot]->{TYPE} = 'ROOT';
	$indexed[$nextRoot]->{'NAME'} = 'Songs';
	$nextRoot++;

	# Extras
	$indexed[$nextRoot]->{TYPE} = 'ROOT';
	$indexed[$nextRoot]->{'NAME'} = 'Extras';
	$extras = 0;
	$nextRoot++;
	
	# Playlists (if any were found)
	if (@{$playlists}[0]->{NODECOUNT} > 0)
	{
		%{$indexed[$nextRoot]} = %{shift @{$playlists}};
		$extras++;
		$nextRoot++;
	}
	
	if ($config->{SHOWRANDOMS} == 1)
	{
		$indexed[$nextRoot]->{TYPE} = 'ROOT';
		$indexed[$nextRoot]->{NAME} = 'Randoms';
		$extras++;
		$nextRoot++;
	}
	
	$nextRoot -= $extras;
	# Now fill out the skeletons
	
	# Supernode
	$indexed[INDEX_ROOT]->{'STARTNODE'} = 1;
	$indexed[INDEX_ROOT]->{'NODECOUNT'} = $nextRoot;
	$nextRoot = INDEX_ROOT + 1;

	# Artists
	$indexed[$nextRoot]->{'STARTNODE'} = scalar @indexed;
	$indexed[$nextRoot]->{'NODECOUNT'} = scalar @artists;
	@{$indexed[$nextRoot]->{'CHILDREN'}} = 
		(scalar @indexed .. scalar @artists + scalar @indexed - 1);
	$artistsStart = $indexed[$nextRoot]->{'STARTNODE'};
	$nextRoot++;

	# Albums
	$indexed[$nextRoot]->{'STARTNODE'} = scalar @indexed + scalar @artists;
	$indexed[$nextRoot]->{'NODECOUNT'} = scalar @albums;
	@{$indexed[$nextRoot]->{'CHILDREN'}} =
		(@{$indexed[$nextRoot - 1]->{'CHILDREN'}}[-1] + 1 .. 
		@{$indexed[$nextRoot - 1]->{'CHILDREN'}}[-1] + scalar @albums);
	$albumsStart = $indexed[$nextRoot]->{'STARTNODE'};
	$nextRoot++;

	# Genres (if any were found)
	if (scalar @genres > 0)
	{
		$indexed[$nextRoot]->{'STARTNODE'} = scalar @indexed + scalar @artists + 
			scalar @albums;
		$indexed[$nextRoot]->{'NODECOUNT'} = scalar @genres; 
		@{$indexed[$nextRoot]->{'CHILDREN'}} =
			(@{$indexed[$nextRoot - 1]->{'CHILDREN'}}[-1] + 1 .. 
			@{$indexed[$nextRoot - 1]->{'CHILDREN'}}[-1] + scalar @genres);
		$genresStart = $indexed[$nextRoot]->{'STARTNODE'};
		$nextRoot++;
	}
	
	# Songs
	$indexed[$nextRoot]->{'STARTNODE'} = scalar @indexed + scalar @artists + 
		scalar @albums + scalar @genres;
	$indexed[$nextRoot]->{'NODECOUNT'} = scalar @{$songs};
	@{$indexed[$nextRoot]->{'CHILDREN'}} =
		(@{$indexed[$nextRoot - 1]->{'CHILDREN'}}[-1] + 1 .. 
		@{$indexed[$nextRoot - 1]->{'CHILDREN'}}[-1] + scalar @{$songs});
	$songsStart = $indexed[$nextRoot]->{STARTNODE};
	$nextRoot++;

	# Extras
	$indexed[$nextRoot]->{'STARTNODE'} = $nextRoot + 1;
	$indexed[$nextRoot]->{'NODECOUNT'} = $extras;
	@{$indexed[$nextRoot]->{'CHILDREN'}} = 
		($nextRoot + 1 .. $nextRoot + $extras);
	$nextRoot++;

	# Playlists (if any were found)
	if (@{$playlists}[0]->{NODECOUNT} > 0)
	{
		$indexed[$nextRoot]->{STARTNODE} = scalar @indexed + 
			scalar @artists + scalar @albums + scalar @genres + 
			scalar @{$songs} + scalar @artistsAllSongs;
		@{$indexed[$nextRoot]->{CHILDREN}} = 
			( $indexed[$nextRoot]->{STARTNODE} .. 
			  $indexed[$nextRoot]->{STARTNODE} +
		      $indexed[$nextRoot]->{NODECOUNT} - 1 );
		$playlistsStart = $indexed[$nextRoot]->{STARTNODE};
		$playlistsCount = $indexed[$nextRoot]->{NODECOUNT};
		$nextRoot++;
	}

	# Random picks
	if ($config->{SHOWRANDOMS} == 1)
	{
		$indexed[$nextRoot]->{TYPE} = 'ROOT';
		$indexed[$nextRoot]->{NAME} = 'Randoms';
		$indexed[$nextRoot]->{STARTNODE} = scalar @indexed + 
			scalar @artists + scalar @albums + scalar @genres + 
			scalar @{$songs} + scalar @artistsAllSongs +
			scalar @{$playlists};
		$indexed[$nextRoot]->{NODECOUNT} = scalar @randoms;
		@{$indexed[$nextRoot]->{CHILDREN}} =
			( $indexed[$nextRoot]->{STARTNODE} .. 
			  $indexed[$nextRoot]->{STARTNODE} +
		      $indexed[$nextRoot]->{NODECOUNT} - 1 );
		$randomsStart = $indexed[$nextRoot]->{STARTNODE};
		$nextRoot++;
	}

	$nextRoot -= $extras;
	@{$indexed[INDEX_ROOT]->{'CHILDREN'}} = (1 .. $nextRoot - 1);

	# Concatenate all of the arrays into one large array
	@indexed = (@indexed, 
				@artists, 
				@albums, 
				@genres, 
				@{$songs}, 
				@artistsAllSongs,
				@{$playlists},
				@randoms);
	
	# Save the start nodes for each type for convenience later
	$artistsAllSongsStart = $songsStart + scalar @{$songs};

	# Now loop through all of the entries and fix up the children arrays so 
	# that they point to the new location in the array of everything
	foreach $entry (@indexed)
	{
		# Artist hash
		if ($entry->{TYPE} eq 'ARTIST')
		{
			# First child is the 'all songs' album
			@{$entry->{CHILDREN}}[0] += $artistsAllSongsStart;
			
			# Fix the rest of the albums
			for ($i = 1; $i < scalar @{$entry->{CHILDREN}}; $i++)
			{
				@{$entry->{CHILDREN}}[$i] += $albumsStart;
			}
		}
		elsif ($entry->{TYPE} eq 'PLAYLIST')
		{
			for ($i = 0; $i < scalar @{$entry->{CHILDREN}}; $i++)
			{
				@{$entry->{CHILDREN}}[$i] += ($playlistsStart 
					+ $playlistsCount);
			}
		}
		elsif ($entry->{TYPE} eq 'ALBUM' ||
			   $entry->{TYPE} eq 'GENRE' ||
			   $entry->{TYPE} eq 'ALLSONGS')
		{
			# Fix up all the rest to point to the songs new location
			for ($i = 0; $i < scalar @{$entry->{CHILDREN}}; $i++)
			{
				@{$entry->{CHILDREN}}[$i] += $songsStart;
			}
		}
#print Dumper($entry);		
		# Put the child node count in each parent
		$entry->{NODECOUNT} = scalar @{$entry->{CHILDREN}} 
			if $entry->{TYPE} ne 'SONG'
			   && $entry->{TYPE} ne 'PLAYLIST'
			   && $entry->{TYPE} ne 'PLAYLISTENTRY'
			   && $entry->{TYPE} ne 'RANDOM'
			   && exists $entry->{CHILDREN};
	}
	
	# Sort albums by name
	@{$indexed[2]->{CHILDREN}} = sort {
		lc($indexed[$a]->{NAME}) cmp lc($indexed[$b]->{NAME}) 
	} @{$indexed[2]->{CHILDREN}};

	# Sort songs by name
	@{$indexed[4]->{CHILDREN}} = sort {
		lc($indexed[$a]->{NAME}) cmp lc($indexed[$b]->{NAME}) 
	} @{$indexed[4]->{CHILDREN}};

	# Return the whole mess to the caller
	# my $ii = 0;
	# foreach my $x (@indexed)
	# {
		# next if $x->{TYPE} eq 'SONG';
		# print "$ii: $x->{TYPE} ";
		# print "count: $x->{NODECOUNT} " if exists $x->{NODECOUNT};
		# print "compare: ", scalar @{$x->{CHILDREN}}, " " 
			# if exists $x->{CHILDREN};
		# print "children: @{$x->{CHILDREN}} " if exists $x->{CHILDREN};
		# print "\n\n";
		# 
		# $ii++;
	# }
	return @indexed;
}

sub findSongs
# ---------------------------------------------------------------------------- #
# findSongs:
#	
#	Recursively scans from the directory passed by the caller looking for mp3
#	files. When found it extracts the tags and, if all of the required tags are
#	present it will add the newly created song hash to an array that will be
#	returned to the caller.
#
#	parm01:		Start directory
#
#	returns:	Array of song hash objects
# ---------------------------------------------------------------------------- #
{
	my $config = shift;
	my @mp3s;
	my @mp3sSorted;
	my $cnt = 0;
	my $bad = 0;
	my $song = {};

	# Subroutine for 'find'. This will select the files that end with .mp3 and
	# extract the tags. If all the required tags are present the song hash will
	# be added to the returned array.
	my $wanted = sub {
		
#		return if $cnt > 500;	# Debug to reduce indexing time during dev.
# TODO: ignore @eaDir directories		
		# .mp3 files
		if (/mp3$/i)
		{
#print Dumper($_);
			# Extract the tags
			my $song = extractTags($_);
			
			# Add the song hash to the returned array
			if ($song)
			{
				push @mp3s, $song;
				
				# Count good songs
				$cnt++;
				&$debugFunc($cnt, "\n") if 0 == $cnt % 500;
			}
			else
			{
				# Count rejected songs
				$bad++;
			}
		}
	};
	
	# Call find to recursively search the directory tree
	find({ no_chdir => 1, wanted => $wanted }, ($config->{MUSICROOT}));

	print "Rejected $bad songs\n";
	
	# Sort the song array by artist, album, track name before returning it
	@mp3sSorted = sort {
		my $cmp;
		
		$cmp = lc($a->{ARTIST}) cmp lc($b->{ARTIST});
		return $cmp unless $cmp == 0;
		
		$cmp = lc($a->{ALBUM}) cmp lc($b->{ALBUM});
		return $cmp unless $cmp == 0;
		
		return $a->{TRACK} cmp $b->{TRACK};
	} @mp3s;
	
	return \@mp3sSorted;
}

sub extractTextTagData
# ---------------------------------------------------------------------------- #
# extractTextTagData:
#	
#	Extracts the text from the passed text tag data. Will convert UTF-16 to 
#	something more useful to perl.
#
#	parm01:		Tag data
#
#	returns:	Text string from tag data
# ---------------------------------------------------------------------------- #
{
	my $frameData = $_[0];

	# Get the UTF-16 flag
	my $flag = unpack('C', substr($frameData, 0, 1));

	# Get the text string
	my $textData = substr($frameData, 1) . "\x00\x00";

	# Convert from unicode if necessary
	if ($flag == 0)
	{
		$textData = decode('ISO-8859-1', $textData);
	}
	elsif ($flag == 1)
	{
		$textData = decode('utf16', $textData);
	}
	elsif ($flag == 2)
	{
		warn "Unsupported flag\n";
	}
	elsif ($flag == 3)
	{
#		my $save = $textData;
		$textData = decode('utf8', $textData);
#		print "now its $save -> $textData\n" if length $save == 4;
	}
	
	# Take only up to first null character
	($textData) = split(/\x00/, $textData);

	# Return a null string if we don't have anything valid
	$textData = "" unless defined $textData;
	
	return $textData;
}

sub extractArtist
# ---------------------------------------------------------------------------- #
# extractArtist:
#	
#	Extracts and returns the artist from the ID3v2 text tag passed.
#
#	parm01:		Tag data
#
#	returns:	Hash containing the artist from the tag.
# ---------------------------------------------------------------------------- #
{
	return ( 'ARTIST' => extractTextTagData($_[0]) );
}

sub extractAlbum
# ---------------------------------------------------------------------------- #
# extractAlbum:
#	
#	Extracts and returns the album name from the ID3v2 text tag passed.
#
#	parm01:		Tag data
#
#	returns:	Hash containing the album name from the tag.
# ---------------------------------------------------------------------------- #
{
	return ( 'ALBUM' => extractTextTagData($_[0]) );
}

sub extractTitle
# ---------------------------------------------------------------------------- #
# extractTitle:
#	
#	Extracts and returns the song title from the ID3v2 text tag passed.
#
#	parm01:		Tag data
#
#	returns:	Hash containing the song title from the tag.
# ---------------------------------------------------------------------------- #
{
	return ( 'NAME' => extractTextTagData($_[0]) );
}

sub extractTrack
# ---------------------------------------------------------------------------- #
# extractTrack:
#	
#	Extracts and returns the track number from the ID3v2 text tag passed.
#
#	parm01:		Tag data
#
#	returns:	Hash containing the track number from the tag padded to a 
# 				length of two with a zero if necessary.
# ---------------------------------------------------------------------------- #
{
	my $frameText = extractTextTagData($_[0]);

	# The track data may be in the format of 1/15 as in track 1 of 15 tracks.
	# Remove the /15
	$frameText =~ s/(\d*)\/.*/$1/;
	
	# .. and make the 1 into 01 (for example)
	$frameText = '0' . $frameText unless length($frameText) == 2;
	return ( 'TRACK' => $frameText );
}

sub extractTime
# ---------------------------------------------------------------------------- #
# extractTime:
#	
#	Extracts and returns the time in seconds from the ID3v2 text tag passed.
#
#	parm01:		Tag data
#
#	returns:	Hash containing the time in seconds from the tag.
# ---------------------------------------------------------------------------- #
{
	my $work = extractTextTagData($_[0]);
	
	$work = int($work / 1000) if $work =~ /^\d+$/;
	return ( 'TIME' => $work );
}

sub extractGenre
# ---------------------------------------------------------------------------- #
# extractGenre:
#	
#	Extracts and returns the genre from the ID3v2 text tag passed.
#
#	parm01:		Tag data
#
#	returns:	Hash containing the genre from the tag.
# ---------------------------------------------------------------------------- #
{
	my @stdGenres = (
		'Blues',
		'Classic Rock',
		'Country',
		'Dance',
		'Disco',
		'Funk',
		'Grunge',
		'Hip-Hop',
		'Jazz',
		'Metal',
		'New Age',
		'Oldies',
		'Other',
		'Pop',
		'R&B',
		'Rap',
		'Reggae',
		'Rock',
		'Techno',
		'Industrial',
		'Alternative',
		'Ska',
		'Death Metal',
		'Pranks',
		'Soundtrack',
		'Euro-Techno',
		'Ambient',
		'Trip-Hop',
		'Vocal',
		'Jazz+Funk',
		'Fusion',
		'Trance',
		'Classical',
		'Instrumental',
		'Acid',
		'House',
		'Game',
		'Sound Clip',
		'Gospel',
		'Noise',
		'AlternRock',
		'Bass',
		'Soul',
		'Punk',
		'Space',
		'Meditative',
		'Instrumental Pop',
		'Instrumental Rock',
		'Ethnic',
		'Gothic',
		'Darkwave',
		'Techno-Industrial',
		'Electronic',
		'Pop-Folk',
		'Eurodance',
		'Dream',
		'Southern Rock',
		'Comedy',
		'Cult',
		'Gangsta',
		'Top 40',
		'Christian Rap',
		'Pop/Funk',
		'Jungle',
		'Native American',
		'Cabaret',
		'New Wave',
		'Psychadelic',
		'Rave',
		'Showtunes',
		'Trailer',
		'Lo-Fi',
		'Tribal',
		'Acid Punk',
		'Acid Jazz',
		'Polka',
		'Retro',
		'Musical',
		'Rock & Roll',
		'Hard Rock',
		'Folk',
		'Folk-Rock',
		'Swing',
		'Fast Fusion',
		'Bebob',
		'Latin',
		'Revival',
		'Celtic',
		'Bluegrass',
		'Avantgarde',
		'Gothic Rock',
		'Progressive Rock',
		'Psychedelic Rock',
		'Symphonic Rock',
		'Slow Rock',
		'Big Band',
		'Chorus',
		'Easy Listening',
		'Acoustic',
		'Humour',
		'Speech',
		'Chanson',
		'Opera',
		'Chamber Music',
		'Sonata',
		'Symphony',
		'Booty Bass',
		'Primus',
		'Porn Groove',
		'Satire',
		'Slow Jam',
		'Club',
		'Tango',
		'Samba',
		'Folklore',
		'Ballad',
		'Power Ballad',
		'Rhythmic Soul',
		'Freestyle',
		'Duet',
		'Punk Rock',
		'Drum Solo',
		'A capella',
		'Euro-House',
		'Dance Hall'
	);
	
	my $frameText = extractTextTagData($_[0]);
	my $index;
	
	if ($index = $frameText =~ /[(]?(\d{1,4})[)]?\D?/)
	{
		if ($index < scalar @stdGenres)
		{
			$frameText = $stdGenres[$index];
		}
		else
		{
			$frameText = "Unknown numeric genre";
		}
	}
	
	return ( 'GENRE' => $frameText );
}

sub extractYear
# ---------------------------------------------------------------------------- #
# extractYear:
#	
#	Extracts and returns the year from the ID3v2 text tag passed.
#
#	parm01:		Tag data
#
#	returns:	Hash containing the year from the tag.
# ---------------------------------------------------------------------------- #
{
	return ( 'YEAR' => extractTextTagData($_[0]) );
}

sub extractTags
# ---------------------------------------------------------------------------- #
# extractTags:
#
#	Extracts the required ID3V2 tags from the specified file and returns them in
#	a hash to the caller.
#
#	This routine is not a complete implementation of ID3V2 tag extraction and 
#	may not support some of the tags in your files. Specifically most character 
#	sets are not supported. However it works for all of my MP3s which are tagged
#	mostly with Amarok.
#
#	One may ask why it is that I implemented my own tag extraction routine. I 
#	did this because all of the perl packages available in CPAN (three that I 
#	found) all failed on one or more of my files. For example one wouldn't read 
#	UTF-16 text tags, one wouldn't support ID3v2.4 tags etc, etc, etc. And what 
#	a pain it was...
#
#	parm01:		File to extract tags from
#
#	returns:	Hash containing relevant ID3V2 information or undef if the 
#				file does not contain all of the required tags.
# ---------------------------------------------------------------------------- #
{	# xxxx rip off root directory
	my $cnt;
	my $buf;
	my $flg1;
	my $flg2;
	my $tagLen;
	my $tagName;
	my $text;
	my $ID3Version;
	my $unsync = 0;
	my $frameStart = 0;
	my $frameLength = 0;
	my $frameData;
	my $return = {};
	my %tagFuncs = (
		'TPE1' => \&extractArtist,
		'TALB' => \&extractAlbum,
		'TIT2' => \&extractTitle,
		'TRCK' => \&extractTrack,
		'TLEN' => \&extractTime,
		'TCON' => \&extractGenre,
		'TYER' => \&extractYear
		);

	# Wrap it all in an eval to trap and process errors
	eval
	{	
		# Open the file and set its mode
		open(MP3IN, $_[0]) or die "ERROR: Cannot open file: $!";
		binmode(MP3IN);
		
		# Read and check for ID3V2 tags constant
		$cnt = read(MP3IN, $buf, 3) or die "ERROR: Unable to read file: $!";
		
		die "UNSUPPORTED: File does not contain an ID3V2 tag\n" unless
			($cnt == 3 and $buf eq "ID3");

		# Read and check the ID3V2 version number
		$cnt = read(MP3IN, $buf, 2) or die "ERROR: Unexpected end of file\n";
		($ID3Version, $flg2) = unpack("CC", $buf);
	
		die "UNSUPPORTED: ID3V2 version is not supported\n" unless
			($ID3Version == 3 || $ID3Version == 4);

		# Read the flags and ensure they are all supported
		$cnt = read(MP3IN, $buf, 1) or die "ERROR: Unexpected end of file\n";
		$flg1 = unpack("C", $buf);

		die "UNSUPPORTED: Unsupported tag attributes present in flags\n" unless
			(($flg1 & 0x60) == 0);
			
		# Check for unsync flag (will cripple performance...)
		$unsync = 1 if ($flg1 & 0x80); 

		# Get length (in unsynchronised form)
		$cnt = read(MP3IN, $buf, 4) or die "ERROR: Unexpected end of file\n";

		# Length + length of header - ignore length of footer if present 
		# because we don't care
		$tagLen = 10 + UnsynchLen($buf);

		if ($flg1 & 0x40)
		{
			# Need to skip extended header
			$cnt = read(MP3IN, $buf, 4) or 
				die "ERROR: Unexpected end of file\n";
			$cnt = UnsynchLen($buf);
			seek(MP3IN, $cnt - 4, 1) or 
				die "ERROR: Unexpected failure seeking past extended header\n";
		}

		# Now search the tags for those that are of interest to us
		while (tell(MP3IN) < $tagLen - 4)
		{
			$frameStart = tell(MP3IN);

			# Read the tag
			$cnt = read(MP3IN, $tagName, 4) or 
				die "ERROR: Unexpected end of file\n";

			if ($tagName eq "\x00\x00\x00\x00")
			{
				# Done with tags. This is filler.
				last;
			}
			elsif (!($tagName =~ /^[A-Z0-9]{4}/))
			{
				# All tags begin with four uppercase characters
				if ($ID3Version < 4 && $unsync)
				{
					# Could be that the frame needs to be unsynced
					# Note this code assumes that the synced frames are not
					# actually required by the code. In my collection none
					# of the text frames have been or need to be synced.
					seek(MP3IN, -4, 1);

					while ($frameData =~ /\xFF(?=[\x00\xE0-\xFF])/g)
					{
						read(MP3IN, $buf, 1);
					}
					
					$unsync = 0;
					next;
				}
				else
				{
					die "ERROR: Logic error - expected a tag value " .
						"<$tagName> at $frameStart in $tagLen ...\n";
				}
			}
			
			# Read and convert the tag length
			$buf = '';
			$cnt = read(MP3IN, $buf, 4) or 
				die "ERROR: Unexpected end of file\n";
			if ($ID3Version >= 4)
			{
				$frameLength = UnsynchLen($buf);
			}
			else
			{
				$frameLength = unpack('N', $buf);
			}
			
			# Read the second flag (first is of no interest to us)
			seek(MP3IN, 1, 1) or
				die "ERROR: Failed to seek past first frame flag\n";
			$cnt = read(MP3IN, $buf, 1) or 
				die "ERROR: Unexpected end of file\n";
			$flg1 = unpack("C", $buf);
			
			# If any of the flags are set in the second flag we pretty much 
			# don't support it
			die "UNSUPPORTED: Frame has unsupported flags set\n" unless
				(($flg1 & 0x0F) == 0);

			# Read all of the frame data
			read(MP3IN, $frameData, $frameLength);
			
			# Add the tag data to the hash
			if (exists($tagFuncs{$tagName}))
			{
				%$return = ( %$return, $tagFuncs{$tagName}->($frameData) );
			}
			
			seek(MP3IN, $frameStart + 10 + $frameLength, 0) or 
				die "ERROR: Unexpected failure seeking next frame\n";
		}
	};

	# Fail if an error occured or there was no artist, album, track number or
	# track title present in the tags. (I'm a stickeler for good tags in my 
	# MP3s - your results may vary.)
	if ($@) 
	{
	    print "Error - $@ in $_[0]\n";
		$return = undef;
	}
	elsif (! exists $return->{ARTIST} || 
		   ! exists $return->{NAME} ||
		   ! exists $return->{TRACK} ||
		   ! exists $return->{ALBUM})
	{
	    print "Error - required tags missing from $_[0]\n";
		$return = undef;
	}
	else
	{
		# Add a couple more tags of interest to the caller
		$return->{TYPE} = 'SONG';
		$return->{FILENAME} = $_[0];
	}

	# Close the file and return.
	close(MP3IN);
	return $return;
}

sub UnsynchLen
# ---------------------------------------------------------------------------- #
# UnsynchLen:
#
#	Calculate the tag block lengt from the unsynchronized bytes in the header. 
#	The total integer size is 28 bits made up of the least significant 7 bits of
#	the four bytes.
#
#	parm01:		Four bytes to calculate the length from.
#
#	returns:	Calculated length 
# ---------------------------------------------------------------------------- #
{
	# Unpack the four bytes
	my @lenAr = unpack("CCCC", $_[0]);
	my $len = 0;
	my $i;

	foreach $i (@lenAr)
	{
		$len = ($len << 7) + ($i & 0x7f);
	}

	# Return the length
	return $len;
}

sub findPlaylists
# ---------------------------------------------------------------------------- #
# findPlaylists:
#	
#	Recursively scans from the directory passed by the caller looking for mp3
#	files. When found it extracts the tags and, if all of the required tags are
#	present it will add the newly created song hash to an array that will be
#	returned to the caller.
#
#	parm01:		Start directory
#
#	returns:	Array of song hash objects
# ---------------------------------------------------------------------------- #
{
	my $config = shift;
	my $playListEntry;
	my $playList;
	my @playListEntries;
	my @playLists;
	my $root = {};
	
	my %playListTypes = ( 'PLS', \&parsepls, 'M3U', \&parsem3u );

	# Subroutine for 'find'. This will select the files that end with .mp3 and
	# extract the tags. If all the required tags are present the song hash will
	# be added to the returned array.
	my $wanted = sub {

		# Playlist files (extension is assumed to be three characters)
		if (substr($_, -4, 1) eq "." && 
			exists $playListTypes{uc(substr($_, -3))})
		{
			# Extract the entries
			&$debugFunc("Processing playlist $_ \n");
			$playListEntry = &{$playListTypes{uc(substr($_, -3))}}($_, $config);
			
			# Add the song hash to the returned array
			if ($playListEntry)
			{
				$playList = {};
				$playList->{TYPE} = 'PLAYLIST';
				$playList->{NAME} = substr(basename($_), 0, 
					length(basename($_)) - 4);
				$playList->{NODECOUNT} = scalar @{$playListEntry};
				@{$playList->{CHILDREN}} = (scalar @playListEntries ..
					scalar @playListEntries + scalar @{$playListEntry} - 1);
				push @playLists, $playList;
				@playListEntries = (@playListEntries, @{$playListEntry});
			}
		}
	};
	
	# Call find to recursively search the directory tree
	find({ no_chdir => 1, wanted => $wanted }, ($config->{MUSICROOT}));
	
	# Build the root node otherwise the index later won't know where the 
	# playlist entries start in the code
	$root->{TYPE} = 'ROOT';
	$root->{'NAME'} = 'Playlists';
	$root->{'NODECOUNT'} = scalar @playLists;

	# Return the whole lot to the caller
	@playLists = ($root, @playLists, @playListEntries);
	return \@playLists;
}

sub parsem3u
# ---------------------------------------------------------------------------- #
# parsem3u:
#	
#	Parses the contents of an m3u playlist file and returns the data to the
#	caller in a hash
#
#	parm01:		Playlist file name
#
#	returns:	An array of playlist entries
# ---------------------------------------------------------------------------- #
{
	my $file = shift;
	my $config = shift;
	my $line;
	my %plsInfo;
	my @plsList;
	
	eval
	{
		open(M3UIN, $file) or die "Open failed $1";
	
		die "No data in file" unless defined ($line = <M3UIN>);
		die "bad header\n" unless $line =~ /^#EXTM3U/;
		
		while ($line = <M3UIN>)
		{
			%plsInfo = ();
			die "bad entry\n" unless $line =~ /^#EXTINF\:(\-?\d+),(.*)/;
			$plsInfo{NAME} = $2;
			$plsInfo{LENGTH} = $1;
			die "Unexpected end of file\n" unless defined ($line = <M3UIN>);
			chomp $line;
			
			$line = resolvePlaylistEntry($line, $config);

#			&$debugFunc("Mangled entry file = $line\n");

			next if substr($line, 0, 7) ne 'http://' && ! -r $line;
				
			$plsInfo{URL} = $line;
			$plsInfo{TYPE} = 'PLAYLISTENTRY';
			%{$plsList[scalar @plsList]} = %plsInfo;
		}
		
		close(M3UIN);
	};
	
	if ($@)
	{
		&$debugFunc("Playlist processing failed $@\n");
		return undef;
	}
	
	if (scalar @plsList == 0)
	{
		&$debugFunc("Playlist $file has no valid entries\n");
		return undef;
	}
		
	return \@plsList;
}

sub parsepls
# ---------------------------------------------------------------------------- #
# parsepls:
#	
#	Parses the contents of an pls playlist file and returns the data to the
#	caller in a hash
#
#	parm01:		Playlist file name
#
#	returns:	Playlist hash object
# ---------------------------------------------------------------------------- #
{
	my $file = shift;
	my $config = shift;
	my $line;
	my $count;
	my $key;
	my $value;
	my %entries;
	my $i;
	my %plsInfo;
	my @plsList;
	
	eval
	{
		open(PLSIN, $file) or die "Open failed $1";
		
		die "No data in file" unless defined ($line = <PLSIN>);
		die "bad header\n" unless $line =~ /\[playlist\]/i;
		
		while (defined ($line = <PLSIN>))
		{
			($key, $value) = $line =~ m{([\w|\d]+)\s*=([^\r\n]+)};
			
			next if ! defined $key;

			die "Invalid .pls file format: $file" unless defined $value;
			
			$entries{lc($key)} = $value;
		}
		
		close PLSIN;
		
		die "No entry count" unless exists $entries{numberofentries};
		
		for ($i = 1; $i <= $entries{numberofentries}; ++$i)
		{
			$key = 'file' . $i;
			die "Missing file $key" unless exists $entries{$key};
			$line = resolvePlaylistEntry($entries{$key}, $config);

#			&$debugFunc("Mangled entry file = $line\n");
			next if substr($line, 0, 7) ne 'http://' && ! -r $line;
				
			$plsInfo{URL} = $line;
			$key = 'title' . $i;
			die "Missing file $key" unless exists $entries{$key};
			$plsInfo{NAME} = $entries{$key};
			$key = 'length' . $i;
			die "Missing length $key" unless exists $entries{$key};
			$plsInfo{LENGTH} = $entries{$key};
			$plsInfo{TYPE} = 'PLAYLISTENTRY';
			%{$plsList[$i - 1]} = %plsInfo;
			%plsInfo = ();
		}
	};
	
	if ($@)
	{
		&$debugFunc("Playlist processing failed $@\n");
		return undef;
	}

	if (scalar @plsList == 0)
	{
		&$debugFunc("Playlist $file has no valid entries\n");
		return undef;
	}
		
	return \@plsList;
}

sub resolvePlaylistEntry
# ---------------------------------------------------------------------------- #
# resolvePlaylistEntry:
#
#	Add the music root to relative paths, forces the file to be searched for in
#	the music root if webgaol is specified.  
#
#	parm01:		File name to test
#	parm02:		Configuration data
#
#	returns:	New file name
#
# ---------------------------------------------------------------------------- #
{
	my $line = shift;
	my $config = shift;
	
	if (lc(substr($line, 0, 7)) ne 'http://')
	{
		if (substr($line, 0, 1) eq '/')
		{
			if ($config->{WEBGAOL} == 1)
			{
				$line = $config->{MUSICROOT} . 
					substr($line, length($config->{MUSICROOT}));
			}
		}
		else
		{
			$line = $config->{MUSICROOT} . $line;
		}
		
		$line = $config->{WEBURL} . $line;
	}
	
	return $line;
}

sub endprog
# ---------------------------------------------------------------------------- #
# endprog:
#
#	Traps a control C from the user, displays a message and exits. Might do 
#	some clean up here in future, or maybe not...
#
#	returns:	nothing
# ---------------------------------------------------------------------------- #
{
	if ($PID == $webServerPid)
	{
		print "Streamium2d: Web server shutting down\n";
	}
	else
	{
		print "Streamium2d: Main process shutting down\n";
		kill INT => $webServerPid if $PID != $webServerPid;
	}
	
	exit;
}

