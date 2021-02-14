final: prev: {
  neofetch = prev.neofetch.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "dylanaraps";
      repo = old.pname;
      rev = "bec3918bfdf383e4695f3308a50e1115a1064c18";
      sha256 = "sha256-pCQrRxjUf+MhMn+pnMVqjbcuBUY1RCLN+8jh2mXLxzA=";
    };
  });
}
