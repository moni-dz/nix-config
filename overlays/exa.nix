final: prev: {
  exa = prev.exa.overrideAttrs (old: rec {
    src = prev.fetchFromGitHub {
      owner = "ogham";
      repo = old.pname;
      rev = "13b91cced4cab012413b25c9d3e30c63548639d0";
      sha256 = "sha256-1c1WAFEl5eLL1B9Jdvnl5WRNsTGtcfn2G5AKEHTYxKM=";
    };

    postInstall = ''
      ${prev.pandoc}/bin/pandoc --standalone -f markdown -t man man/exa.1.md > man/exa.1
      ${prev.pandoc}/bin/pandoc --standalone -f markdown -t man man/exa_colors.5.md > man/exa_colors.5
      installManPage man/exa.1 man/exa_colors.5
      installShellCompletion \
        --name exa completions/completions.bash \
        --name exa.fish completions/completions.fish \
        --name _exa completions/completions.zsh
    '';

    patches = [ ];

    cargoDeps = old.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "sha256-ncneFEdoDWvGT7ojkymXGbaQ3LZJwyENzK15eHvbEZo=";
    });
  });
}
