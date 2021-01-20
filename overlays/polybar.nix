final: prev: {
  polybar = prev.polybar.override {
    i3Support = false;
    i3GapsSupport = false;
    alsaSupport = false;
    iwSupport = true;
    githubSupport = false;
    mpdSupport = true;
    nlSupport = false;
    pulseSupport = true;
  };
}
