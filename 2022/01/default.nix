{ pkgs ? import <nixpkgs> { } }:

pkgs.stdenv.mkDerivation {
  name = "aoc-2022-1";
  buildInputs = [ pkgs.gfortran ];

  src = ./.;

  buildPhase = ''
    gfortran -o aoc-2022-1 main.f90
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp aoc-2022-1 $out/bin/
  '';
}
