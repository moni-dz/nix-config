final: prev: {
  mpd = prev.mpd.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "MusicPlayerDaemon";
      repo = "MPD";
      rev = "v0.22.6";
      sha256 = "sha256-Xu+MxMxR5u++R3lZHe6UQ+mEmRnWbN6173ZX39KS1A8=";
    };
  });
}
