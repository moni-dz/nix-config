final: prev:

let stdenv = prev.clangStdenv;
in
{
  output-fonts = prev.callPackage ../derivations/output-fonts.nix {
    inherit (prev) stdenvNoCC unzip requireFile;
  };

  phocus = prev.callPackage ../derivations/phocus.nix {
    inherit stdenv;
    inherit (prev) sass fetchFromGitHub;
    theme = import ../config/theme.nix;
  };

  spotify-adblock = prev.callPackage ../derivations/spotify-adblock.nix {
    inherit (prev) writeShellScriptBin spotify;
    spotify-adblock-linux = final.spotify-adblock-linux;
  };

  spotify-adblock-linux = prev.callPackage ../derivations/spotify-adblock-linux.nix {
    inherit (prev) fetchurl;
  };
}
