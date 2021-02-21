final: prev: {
  slock = prev.slock.overrideAttrs (old: {
    src = ./sources/slock.tar.gz;
    buildInputs = (old.buildInputs or [ ]) ++ [ prev.imlib2 ];
  });
}
