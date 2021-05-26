final: prev: {
  slock = prev.slock.overrideAttrs (old: {
    src = prev.slock-src;
    patches = [ ./patches/slock_patch.diff ];
  });
}
