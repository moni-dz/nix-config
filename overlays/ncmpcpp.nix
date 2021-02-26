final: prev: {
  ncmpcpp = prev.ncmpcpp.override {
    visualizerSupport = true;
  };
  master.ncmpcpp = prev.master.ncmpcpp.override {
    visualizerSupport = true;
  };
}
