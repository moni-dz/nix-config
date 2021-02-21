final: prev: {
  slock = prev.slock.overrideAttrs (old: {
    src = ./sources/slock.tar.gz;
    buildInputs = (old.buildInputs or [ ]) 
    ++ (with prev; [
          imlib2
          linux-pam
        ]);
  });
}
