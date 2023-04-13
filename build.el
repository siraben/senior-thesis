(require 'ox-latex)

(setq org-latex-listings 'minted)
(setq org-latex-packages-alist '(("" "minted")))

(add-to-list 'org-latex-classes
             '("scrartcl" "\\documentclass[11pt]{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-default-class "scrartcl")
(setq org-latex-default-packages-alist nil)

(find-file "main.org")
(org-latex-export-to-latex)
(kill-buffer)
