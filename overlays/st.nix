final: prev: {
  st = prev.st.overrideAttrs (old: {
    src = ./tarballs/st.tar.gz;
    buildInputs = (old.buildInputs or [ ]) ++ (with prev; [ harfbuzzFull ]);
  });
}
