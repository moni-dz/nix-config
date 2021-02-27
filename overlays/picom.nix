final: prev: {
  picom = prev.picom.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "yshui";
      repo = old.pname;
      rev = "next";
      sha256 = "sha256-KVNiax2rqPhcVHdkMhVM5Wi76kKQ2IYeu4tO4Y0P6XI=";
    };
  });
}
