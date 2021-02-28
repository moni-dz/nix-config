final: prev: {

  eww = prev.callPackage ../derivations/eww.nix {
    rustPlatform = prev.rustPlatform;
    fetchFromGitHub = prev.fetchFromGitHub;
    rust = prev.rust-bin.nightly."2021-02-21".rust;
    pkgs = prev;
  };
  phocus = prev.callPackage ../derivations/phocus.nix {
    sass = prev.sass;
    fetchFromGitHub = prev.fetchFromGitHub;
    stdenv = prev.stdenv;
    theme = (import ../config/theme.nix);
  };
  output-fonts = prev.callPackage ../derivations/output-fonts.nix {
    stdenv = prev.stdenv;
    unzip = prev.unzip;
    requireFile = prev.requireFile;
  };
}
