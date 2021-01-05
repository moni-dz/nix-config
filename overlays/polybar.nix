final: prev: {
  polybar = prev.polybar.override {
    i3Support = false;
    i3GapsSupport = false;
    alsaSupport = false;
    iwSupport = false;
    githubSupport = false;
    mpdSupport = false;
    nlSupport = false;
    pulseSupport = true;
  };
}
