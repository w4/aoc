# must use --option sandbox false
{ pkgs ? import <nixpkgs> { } }:
pkgs.stdenv.mkDerivation rec {
  name = "aoc-2022-5";
  src = ./.;

  phases = [ "buildPhase" ];
  doCheck = true;

  buildInputs = with pkgs; [ opentofu jinja2-cli ];

  buildPhase = ''
    cp -r $src/* .
    jinja2 main.tf.jinja2 > main.tf

    tofu init
    tofu apply -auto-approve -lock=false -var=outpath=$out
  '';
}
