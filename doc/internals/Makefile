# Makefile to format CCL user manual.
#
# The manual is written in a notation called CCLDoc. Its GitHub project
# may be found at https://github.com/Clozure/ccldoc

# The CCL you want to use.
CCL=ccl

# Directory where your checkout of CCLDoc is.
# Get CCLDoc with:
#   git clone https://github.com/Clozure/ccldoc.git
CCLDOC_ROOT=~/ccl/ccldoc

CSS=../manual/style.css

ccl.html: *.ccldoc $(CSS)
	$(CCL) --batch \
	 -e "(require :asdf)" \
	 -e "(push \"$(CCLDOC_ROOT)/source/\" asdf:*central-registry*)" \
	 -e "(asdf:load-system :ccldoc)" \
	 -e '(defvar *d* (ccldoc:load-document "ccl:doc;internals;internals.ccldoc"))' \
	 -e '(ccldoc:output-html *d* "internals.html" :stylesheet "ccl:doc;manual;style.css")' \
	 -e '(quit)'

