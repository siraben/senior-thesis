{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    emacs
    texlive.combined.scheme-full
    biber
  ];
}
