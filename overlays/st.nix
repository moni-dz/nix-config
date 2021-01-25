final: prev: {
  st = prev.st.overrideAttrs (old: {
    src = ./sources/st.tar.gz;
    buildInputs = (old.buildInputs or [ ]) ++ (with prev; [ harfbuzzFull ]);
  });
}
