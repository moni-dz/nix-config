final: prev: {
  alacritty = prev.alacritty.overrideAttrs (old: rec {
    src = prev.fetchFromGitHub {
      owner = "zenixls2";
      repo = "alacritty";
      rev = "ligature";
      sha256 = "sha256-FiXlIPCFwZUq6NUbbWF6iFjRe3GC/TxisSd0MoEtHoA=";
    };
    cargoDeps = old.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "sha256-BajEa17IIaAL66i+n+fzQxXjdMunpdhGTG7P/IxABrg=";
    });
  });
}
