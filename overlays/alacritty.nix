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
      outputHash = "1g0414pn5h9a0xacksgls7nq1p64x8sv4y042d5y3wf7xmyd0y9w";
    });
  });
}
