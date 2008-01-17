This directory contains various third-party opensourced
system-building tools.

The code here is current as of February 1, 2005; you may want
to check the originating project's homepages to see if more recent
versions are available.

"defsystem.lisp" is part of the clocc project on SourcForge:
<http://sourceforge.net/projects/clocc>.  It's a "system definition
facility" which provides functionality similar to that offered by
the Unix "make" program.  It was originally written by Mark Kantrowitz
and has been maintained and enhanced by many people; I believe that
Marco Antoniotti is currently the principal developer.  This is
version 3.4i of DEFSYSTEM (which is often called "MK-DEFSYSTEM").
Note that, for historical reasons, DEFSYSTEM will try to redefine
the CL:REQUIRE function.

"asdf.lisp" is Another System Definition Facility and is available as
part of the cclan project on SourceForge:
<http://sourceforge.net/projects/cclan>.  It was written by and
is maintained by Daniel Barlow.

"asdf-install" is a library which can be used to download CL packages
from the Internet and which uses ASDF to build and install them.  It's
also part of the cclan project and was originally written (for SBCL)
by Dan Barlow.  It's since been ported to several other CL
implementations; Marco Baringer did the OpenMCL port.

There's excellent documentation on asdf-install in the asdf-install/doc
directory.  As that document mentions, asdf-install is designed to use
the GnuPG package to validate cryptographic signatures associated with
asdf-install-able packages, though it can apparently be configured to
work in an environment in which GnuPG is not available.

Downloading code from publicly-writable Internet sites - without the
ability to verify that that code's really what it claims to be and
from the author who claims to have provided it - is obviously a
dangerous and unwise thing to do.  It's strongly recommended that
people ensure that GnuPG is installed (and ensure that asdf-install is
configured to use it) before using asdf-install to download packages.

(GnuPG packages for OSX are available from <http://macgpg.sourceforge.net>.
Most Linux distributions offer GnuPG through their packaging system;
further information on GnuPG is available at <http:///www.gnupg.org>.


