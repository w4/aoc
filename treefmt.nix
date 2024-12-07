{ pkgs, ... }:
{
  projectRootFile = "flake.nix";

  programs = {
    hlint.enable = true;
    ormolu.enable = true;
    nixpkgs-fmt.enable = true;
    statix.enable = true;
    rustfmt.enable = true;
    fprettify.enable = true;
    jsonnetfmt.enable = true;
    shellcheck.enable = true;
    ocamlformat.enable = true;
  };

  settings.formatter.shellcheck.options = [ "-s" "bash" ];
  settings.formatter.ocamlformat.options = [
    "--enable-outside-detected-project"
    "--profile=ocamlformat"
    "--doc-comments-padding=0" # required for shebangs
  ];
}
