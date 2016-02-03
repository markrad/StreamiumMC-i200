# Streamium MC-i200 Server
##Perl script to feed songs to a Streamium MC-i200

So I'm just throwing this piece of code up on GitHub. Maybe someone somewhere will find it useful. It's been kicking around on my website for years and I still use it today.

##Credits

Credit and thanks given to Nathan Peterson for his work in hacking the protocol. Some of his original pclink.pl code used here.  Visit Nathan's website at http://www.siteswap.org/streamium/

Credit and thanks also given to Dave Witt for the script that I used as a basis to this script. Some of his code can still be seen in here. Visit Dave's website at http://www.witt.tv/streamiumd

##Description

This program is designed to feed your Streamium MC-i200 with MP3s from your collection. Philips, in their wisdom, implemented a non-standard UPNP in that device even though the box said it was UPNP. This forced the user to use the (rather lame) Philips software to feed it. Having realised that the only reason I was running a Windows box (well at the time anyway) was to run that piece of software I decided to look around for a Linux alternative. I came across Dave Witt's code, tried it and it worked but for my usage it had some drawbacks.

1. It did not read the ID3 tag information but rather relied upon a directory structure.
2. It would not give me albums, genres and songs on my Streamium 

So this is my implementation of the same thing. It will provide the Streamium with the option of Artist, Albums, Genres or songs. Each artist, as with the orginial Philips software, will expand into a list of their albums and 'all songs'. It should happily talk to multiple clients but I haven't tested it. It also supports the super scroll invoked by using the remote to enter a letter and skip to the earliest in the list that begins with it.

I've run this on Ubuntu Gusty Gibbon and Debian Etch. You may need some Perl modules from CPAN. Sys::HostIP was not installed on either of my systems.

Currently I run this script on a Synology NAS server.

Unfortunately none of the available perl packages for reading MP3 tags would work with all of my MP3s. I tried three and all of them had some problem with some tags so I wrote my own code. Hopefully this situation will improve because I don't really want to maintain the tag reading code. Your results may vary.

Comments and feedback are welcome.  If you like this software, let me know!

By the way I am not a particularly experienced Perl programmer so if you have a problem with the way I've coded things just be nice.

To do:
* Find an MP3 package that works
* Possibly add support for a UPNP backend instead of the file system - I'm thinking about that. It can be done, I've seen a script for it.
* Add ShoutCast support (which will probably never happen now)
* Treat 'Various' albums differently (which will also probably never happen now)

Version 1.0.a: May 03 2008

* Pretty much a rewrite so gave it a new name and version

Version 1.0.b: August 23 2008

* Initial release

Version 1.0.c: August 24 2008

* Moved indexing of songs to run after daemonizing to speed boot up on systems that load it as part of the initialization. (I do and the old version sucked)

Version 1.0.d: September 08 2008

* Added the option to serve songs from an external web server and suppress the running of the internal web server. See option --urlprefix.
* Centralized the handling of configuration options by putting them into a hash and passing it to those who need it. 
* Massively reduced dependancy on global variables. 
* Deleted unused variables.
* Added persistent index on disk to speed start up with rebuild option.
* Added support for playlists.

Version 1.0.e: October 30 2008

* Added all of the ID3V1 standard genres to the genre interpreter.
* Added playlist support
* Added random song & random album support. This made a fairly radical change to the UI on the Streamium. It will only support a maximum of five top level nodes. Node five is now Extras with sub-nodes of Playlists and Randoms.
* Fixed code to generate valid XML
* Ensured text from ID3 tags is decoded properly
* Write out PID file for easier daemon control
* Kill web server when main is terminated
* Canned the idea of a list of played files. Too difficult to implement in my design for the value it gives.

Version 1.0.f: March 24 2009

* Fixed index file option bug
* Added 'index and quit' option
* Made options case sensitive
* Modified pidfile option to -Pidfile

Version 1.0.g: February 01 2012

* Fixed bug that occured when there were no playlists to index
* Modified code to use functionality available on a Synology NAS device
