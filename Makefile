ORG=main.org
TEX=main.tex
BIB=citations.bib
EMACS=emacs
EMACS_COMMAND=$(EMACS) --batch --load build.el

.PHONY: all clean

all: main.pdf

$(TEX): $(ORG)
	$(EMACS_COMMAND)

main.pdf: $(TEX) $(BIB)
	latexmk -pdf -pdflatex='pdflatex -shell-escape -interaction nonstopmode' $<

clean:
	latexmk -C
	rm -f *.aux *.bbl *.blg *.log *.out *.vtc *.fdb_latexmk *.fls
