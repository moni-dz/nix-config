final: prev: {
  slock = prev.slock.overrideAttrs (old: {
    src = ./tarballs/slock.tar.gz;
    buildInputs = (old.buildInputs or [ ]) ++ [ prev.imlib2 ];
  });
}
