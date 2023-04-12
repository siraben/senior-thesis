ORG=main.org
TEX=main.tex
BIB=citations.bib
EMACS=emacs
EMACS_INIT_FILE=~/.emacs.d/init.el
EMACS_COMMAND=$(EMACS) --batch -l $(EMACS_INIT_FILE)

.PHONY: all clean

all: main.pdf

$(TEX): $(ORG)
	$(EMACS_COMMAND) -eval "(progn (find-file \"$<\") (org-latex-export-to-latex nil nil nil t))" -kill

main.pdf: $(TEX) $(BIB)
	latexmk -pdf -pdflatex='pdflatex -shell-escape -interaction nonstopmode' $<

clean:
	latexmk -C
	rm -f *.aux *.bbl *.blg *.log *.out *.vtc *.fdb_latexmk *.fls
	rm -f abstract.txt
