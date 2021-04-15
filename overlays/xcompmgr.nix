final: prev: {
  xcompmgr = prev.xcompmgr.override { stdenv = prev.clangStdenv; };
}
