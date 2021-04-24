final: prev: {
  haskellPackages = prev.haskellPackages.extend (prev.haskell.lib.packageSourceOverrides {
    xmonad = prev.fetchFromGitHub {
      owner = "xmonad";
      repo = "xmonad";
      rev = "46f637e0bed18fa09e46e8f8ad5ccd0ae19d6fa0";
      sha256 = "sha256-oCwxyxMbo/LEbQQlw0LnopMnLSysarV/HMcpeK3mVgY=";
    };

    xmonad-contrib = prev.fetchFromGitHub {
      owner = "xmonad";
      repo = "xmonad-contrib";
      rev = "0ebd3a0534f1b4cdb0aa931bf16b296e557dd811";
      sha256 = "sha256-v36LYi7muTz/6u2Y7Kdkv73TeM0LmqKhO313Cyvb2jg=";
    };
  });
}
