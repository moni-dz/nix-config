final: prev: {
  polybar = prev.poly.polybar.override {
    stdenv = prev.clangStdenv;
    i3Support = false;
    i3GapsSupport = false;
    alsaSupport = false;
    iwSupport = false;
    githubSupport = false;
    mpdSupport = true;
    nlSupport = true;
    pulseSupport = true;
  };
}
