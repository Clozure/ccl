test:
	echo "(load \"load-ba.lsp\") (in-package :ba-test) (rt:do-tests)" | $(LISP) | tee test.out

clean:
	@rm -f test.out *.cls *.fasl *.o *.so *~ *.fn *.x86f *.fasl *.ufsl *.abcl *.fas *.lib \#*\#
	@rm -f gazonk* out.class *.dfsl *.d64fsl
