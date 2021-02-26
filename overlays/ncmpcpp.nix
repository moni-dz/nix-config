final: prev: {
  ncmpcpp = prev.ncmpcpp.override {
    visualizerSupport = true;
    clockSupport = true;
    taglibSupport = true;
  };
  master.ncmpcpp = prev.master.ncmpcpp.override {
    visualizerSupport = true;
    clockSupport = true;
    taglibSupport = true;
  };
}
