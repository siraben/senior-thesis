TEX=$(wildcard *.tex)
BIB=$(wildcard *.bib)

main.pdf: $(TEX) $(BIB)
	latexmk -pdf -pdflatex='pdflatex -shell-escape -interaction nonstopmode' main.tex

abstract.txt: abstract.tex
	@sed -e 's/\\begin{abstract}//g'  \
       -e 's/\\end{abstract}//g'    \
       -e 's/\\vspace{.*}//g'       \
       -e 's/\\todo{.*}//g'         \
       -e 's/\\label{.*}//g'        \
       -e 's/\\looseness=[^ ]*//g'  \
       -e 's/\\[a-z]*{\(.*\)}/\1/g' \
       -e 's/``/"/g'                \
       -e "s/''/\"/g"               \
       -e 's/\\//g'                 \
       -e '/^\s*%.*/d'              \
       -e 's/^\([^%]*\)%.*/\1/g'		\
       -e 's/^ *//g'                \
       -e 's/  */ /g'               \
       $^ | cat -s | fmt -w 72 > $@

.PHONY: clean
clean:
	latexmk -C
	rm -f *.aux *.bbl *.blg *.log *.out *.vtc *.fdb_latexmk *.fls
	rm -f abstract.txt


