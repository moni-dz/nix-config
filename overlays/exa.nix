final: prev: {
  exa = prev.exa.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "ogham";
      repo = old.pname;
      rev = "13b91cced4cab012413b25c9d3e30c63548639d0";
      sha256 = "sha256-1c1WAFEl5eLL1B9Jdvnl5WRNsTGtcfn2G5AKEHTYxKM=";
    };

    patches = [ ];

    cargoSha256 = prev.lib.fakeSha256;
  });
}
