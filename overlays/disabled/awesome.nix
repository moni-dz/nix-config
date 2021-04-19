final: prev: {
  awesome = (prev.awesome.overrideAttrs (old: {
    lua = prev.luajit;
    inherit (prev.luajitPackages) ldoc lgi;

    src = prev.fetchFromGitHub {
      owner = "awesomeWM";
      repo = old.pname;
      rev = "fda950d186efdc43a64c4300a27a5e19a55c2bd7";
      sha256 = "sha256-mHnApQQwuN1KfL8crus7pNNay3jSc0GpprcuBvrjXHU=";
    };
  })).override {
    stdenv = prev.clangStdenv;
    gtk3Support = true;
  };
}
