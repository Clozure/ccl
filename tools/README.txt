This directory contains various third-party system-building tools.

It is possible that more recent versions of this software may be
availabe from the web sites of the originating projects.

"asdf.lisp" is Another System Definition Facility.  It is available
from <http://common-lisp.net/project/asdf/>.  It hooks into CCL's
existing CL:REQUIRE function.

"defsystem.lisp" is part of the clocc project on SourceForge:
<http://sourceforge.net/projects/clocc>.  It's a "system definition
facility" which provides functionality similar to that offered by the
Unix "make" program.  It was originally written by Mark Kantrowitz and
has been maintained and enhanced by many people; I believe that Marco
Antoniotti was the last maintainer.  This is version 3.4i of DEFSYSTEM
(which is often called "MK-DEFSYSTEM").  Note that, for historical
reasons, DEFSYSTEM will try to redefine the CL:REQUIRE function.
