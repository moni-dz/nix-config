final: prev: {
  picom = prev.picom.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "yshui";
      repo = "picom";
      rev = "next";
      sha256 = "sha256-bgsRA6i6R5JZWhZ2firy5JWs70DfyqYvatTgyA82ZOo=";
    };
  });
}
