final: prev: {
  weechat-unwrapped = prev.weechat-unwrapped.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "weechat";
      repo = "weechat";
      rev = "master";
      sha256 = "sha256-pPU+vyQC+Cx7TxW0DU2x7J5T2G3oDrkKYpPQK/iWv3c=";
    };
  });
}
