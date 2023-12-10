{ pkgs ? import <nixpkgs> { } }:
pkgs.stdenv.mkDerivation rec {
  name = "aoc-2022-4";
  src = ./.;

  phases = [ "buildPhase" ];
  doCheck = true;

  buildInputs = with pkgs; [ go-jsonnet ];

  buildPhase = ''
    jsonnet $src/4.jsonnet > $out
  '';
}
