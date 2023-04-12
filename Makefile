ORG=main.org
TEX=main.tex
BIB=citations.bib
BBL=main.bbl
EMACS=emacs
EMACS_COMMAND=$(EMACS) --batch --load build.el

.PHONY: all clean

all: main.pdf

$(TEX): $(ORG)
	$(EMACS_COMMAND)

$(BBL): $(TEX) $(BIB)
	latexmk -pdf -pdflatex='pdflatex -shell-escape -interaction nonstopmode' $<
	biber main

main.pdf: $(TEX) $(BIB) $(BBL)
	latexmk -pdf -pdflatex='pdflatex -shell-escape -interaction nonstopmode' $<

clean:
	latexmk -C
	rm -f *.aux *.bbl *.blg *.log *.out *.vtc *.fdb_latexmk *.fls
	rm -f abstract.txt

