final: prev: {
  output-fonts = prev.callPackage ../derivations/output-fonts.nix {
    inherit (prev) stdenvNoCC unzip requireFile;
  };

  phocus = prev.callPackage ../derivations/phocus.nix {
    inherit (prev) stdenvNoCC fetchFromGitHub sass;
    theme = import ../config/theme.nix;
  };

  spotify-adblock = prev.callPackage ../derivations/spotify-adblock.nix {
    inherit (prev) fetchurl;
  };

  spotify-wrapped = prev.callPackage ../derivations/spotify-wrapped.nix {
    inherit (prev) writeShellScriptBin spotify;
    spotify-adblock = final.spotify-adblock;
  };

  taiwins = prev.callPackage ../derivations/taiwins.nix {
    inherit (prev) stdenv fetchFromGitHub meson ninja pkg-config wayland cairo fontconfig freetype libdrm libinput libGL libxkbcommon linux-pam lua5_3 mesa wayland-protocols xwayland pixman;
    inherit (prev.gnome3) librsvg;
    inherit (prev.xorg) libX11 libxcb;
  };
}
