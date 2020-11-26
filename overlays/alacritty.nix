self: super:
{
  alacritty = super.alacritty.overrideAttrs (old: rec {
    src = super.fetchFromGitHub {
      owner = "alacritty";
      repo = "alacritty";
      rev = "a2727d06f77973c47e1909a5f426789d6531bda9";
      sha256 = "189fyhvab5s6615bszr6fwidagb9wi2nxl2a8hsi9v1fx14svi3n";
    };
    cargoDeps = old.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "01hd6qjh8q7s94i7l226xwfa1277l8lb3681sx34z64xz57wq6sv";
    });
  });
}
