final: prev: {
  ncmpcpp = prev.ncmpcpp.override {
    visualizerSupport = true;
    clockSupport = false;
  };
}
