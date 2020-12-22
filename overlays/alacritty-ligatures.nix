final: prev: {
  alacritty = prev.alacritty.overrideAttrs (old: rec {
    src = prev.fetchFromGitHub {
      owner = "zenixls2";
      repo = "alacritty";
      rev = "ligature";
      sha256 = "sha256-g1GJ70OWmfpr3oopBbinOT7LUjehNCsW/6iQUQN+t7Q=";
    };
    buildInputs = (old.buildInputs or []) ++ [ prev.gcc6 ];
    cargoDeps = old.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "sha256-BajEa17IIaAL66i+n+fzQxXjdMunpdhGTG7P/IxABrg=";
    });
  });
}
