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

ccl.html: *.ccldoc style.css
	$(CCL) --batch \
	 -e "(require :asdf)" \
	 -e "(push \"$(CCLDOC_ROOT)/source/\" asdf:*central-registry*)" \
	 -e "(asdf:load-system :ccldoc)" \
	 -e '(defvar *d* (ccldoc:load-document "ccl:doc;manual;ccl.ccldoc"))' \
	 -e '(ccldoc:output-html *d* "ccl.html" :stylesheet "ccl:doc;manual;style.css")' \
	 -e '(quit)'

.PHONY: check
check:
	@corrected="$$(grep -F -c "(some #'(lambda (n) (not (numberp n))) (car arglist))" debugging.ccldoc)"; \
	 buggy="$$(grep -F -c "(some #'(lambda (n) (not (numberp n))) arglist)" debugging.ccldoc)"; \
	 test "$$corrected" -eq 2 && test "$$buggy" -eq 0 || { \
	   printf '%s\n' 'Both ADVISE examples must inspect (car arglist), not arglist' >&2; \
	   exit 1; \
	 }

