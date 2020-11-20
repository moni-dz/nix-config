self: super:
{
  st = super.st.overrideAttrs (old: {
    src = super.fetchFromGitHub {
      owner = "fortuneteller2k";
      repo = "st";
      rev = "404bd376f4836499e3dc404c670778743038c2e9";
      sha256 = "0809xxmvgniwjg3ql5zsfhfwv6fdw7c6ajqscxb49lb4rivsal1q";
    };
    buildInputs = (old.buildInputs or []) ++ [ self.harfbuzzFull ];
    patches = [];
  });
}
