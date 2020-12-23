final: prev: {
  qutebrowser = prev.qutebrowser.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "qutebrowser";
      repo = "qutebrowser";
      rev = "db70f34ba3d06d5abe2d0af667cafe0123c80dba";
      sha256 = "0000000000000000000000000000000000000000000000000000";
    };
  });
}
