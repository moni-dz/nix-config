final: prev: {
  ncmpcpp = prev.ncmpcpp.override {
    stdenv = prev.clangStdenv;
    visualizerSupport = true;
  };
}
