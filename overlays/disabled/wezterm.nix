final: prev: {
  wezterm = prev.wezterm.overrideAttrs (old: rec {
    version = "2021-05-12-nightly";

    src = prev.fetchFromGitHub {
      owner = "wez";
      repo = "wezterm";
      rev = "a59e9b1706be78beb722c7c6362687e13c0ec30b";
      sha256 = "sha256-oThLbVYwiOPnJkvnLNr3k5nLGNuqs+/Sk8kpEj+JoB4=";
    };

    cargoDeps = old.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "sha256-DALJZVK3P/yXLKvtAn5qHSfwPxt8psBarU3R6WBHPCc=";
    });
  });
}
