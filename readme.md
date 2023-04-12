# Formalizing graph theory in type theory
![Build status](https://github.com/siraben/senior-thesis/actions/workflows/build.yml/badge.svg)

This repository contains the source files for a formalization of graph
theory in type theory, including a review of various formalization
approaches found in the literature, an implementation of Wigderson's
graph coloring algorithm, and other related materials.

## Abstract
Despite the rich theory and extensive applications of graph theory in
computer science and mathematics, formal developments of graph theory
have mostly been restricted to specific applications or definitions of
graphs. In this work, we present progress towards the formalization of
Wigderson's graph coloring algorithm through our own novel
approach. We also provide a comprehensive review of various
formalization approaches found in the literature, examining their
motivations, theoretical design choices, and the robustness of their
conclusions.

## Requirements
- Emacs 26 or later
- LaTeX distribution (e.g., TeX Live, MacTeX)

## Usage
To generate the PDF file from the `.org` source file, simply run
make. This will generate the `.tex` file using Emacs, and then compile
it to a PDF using `latexmk`.

## Credits
This work was written by Siraphob (Ben) Phipathananunth.

## License
This work is licensed under the MIT License. See the `LICENSE` file
for details.
