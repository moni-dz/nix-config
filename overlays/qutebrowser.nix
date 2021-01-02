final: prev: {
  qutebrowser = prev.qutebrowser.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "qutebrowser";
      repo = "qutebrowser";
      rev = "master";
      sha256 = "sha256-nnKA5wjQbF+OuBV1ecoNFPpg3CO/yDNgQv8cTvdKtd8=";
    };

    propagatedBuildInputs = (old.propagatedBuildInputs or []) ++ (with prev; [
      python38Packages.importlib-resources
    ]);
  });
}
