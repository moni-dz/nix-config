final: prev: {
  ncmpcpp = prev.ncmpcpp.override {
    stdenv = prev.clangStdenv;
    visualizerSupport = true;
  };
  ncmpcpp-artwork = (prev.ncmpcpp.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "evanc577";
      repo = "ncmpcpp";
      rev = "39c408b4e7545da113422a93cdeb88f59151b81a";
      sha256 = "sha256-PGyZPtJepkVoVe7Um48UMjk3CjqgM4UWQD6IwL/ifYc=";
    };

    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ (with prev; [ autoreconfHook ]);

    buildInputs = (old.buildInputs or [ ]) ++ (with prev; [
      imagemagick
      ueberzug
    ]);
  })).override {
    stdenv = prev.clangStdenv;
    visualizerSupport = true;
  };
}
