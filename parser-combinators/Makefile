INPUTTEX  = $(shell grep "\\\\input{" parsers.lhs | sed "s/^.*input{\(.*\)}.*$$/\1.tex/")
INPUTHTML  = $(shell grep "\\\\input{" parsers.lhs | sed "s/^.*input{\(.*\)}.*$$/\1.html/")

pdf : parsers.lhs $(INPUTTEX)
	pandoc -s -i $< -o parsers.pdf

web : parsers.lhs $(INPUTTEX)
	pandoc -s -i $< -o parsers.html

%.tex : %.lhs
	pandoc -i $< -o $@

%.html : %.lhs
	pandoc -i $< -o $@

	


.PHONY : clean
clean :
	rm -f $(INPUTTEX)
	rm -f $(INPUTHTML)
