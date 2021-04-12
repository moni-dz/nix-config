final: prev: {
  picom = (prev.picom.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "yshui";
      repo = old.pname;
      rev = "468d8a0879d619f3028ea371c1b3ad34c6ae2183";
      sha256 = "sha256-HY4xMh33ycWzlK3bo7jrQm3uVn4toO56u8qnYKmdxW0=";
    };
  })).override { stdenv = prev.clangStdenv; };
}
