final: prev:

let
  stdenv = prev.clangStdenv;
in
{
  output-fonts = prev.callPackage ../derivations/output-fonts.nix {
    inherit stdenv;
    unzip = prev.unzip;
    requireFile = prev.requireFile;
  };
  phocus = prev.callPackage ../derivations/phocus.nix {
    inherit stdenv;
    sass = prev.sass;
    fetchFromGitHub = prev.fetchFromGitHub;
    theme = (import ../config/theme.nix);
  };
}
