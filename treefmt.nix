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
  };

  settings.formatter.shellcheck.options = [ "-s" "bash" ];
}
