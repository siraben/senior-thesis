{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.emacs
    pkgs.stow
    pkgs.texlive.combined.scheme-full
  ];
}
