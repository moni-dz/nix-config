final: prev: {
  polybar = (prev.polybar.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = old.pname;
      repo = old.pname;
      rev = "2901e1e4769e8ffcf2def9ced9b7bda02763294c";
      sha256 = "sha256-YDFANXiGRTQOJ4peH8hCUk90NauqCG2eiigxRAVpa2A=";
      fetchSubmodules = true;
    };

    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ prev.python3Packages.sphinx ];
  })).override {
    stdenv = prev.clangStdenv;
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
