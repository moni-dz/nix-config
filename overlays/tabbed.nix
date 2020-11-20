self: super:
{
  tabbed = super.tabbed.overrideAttrs (old: {
    src = super.fetchFromGitHub {
      owner = "fortuneteller2k";
      repo = "tabbed";
      rev = "f78d68dcaca59a728e18072ff5f6d7dc7bf12a8a";
      sha256 = "112f45hpbyzz40zgpb68jxj3vcryks0d5szzcf0lj2j02sbqp987";
    };
    patches = [];
  });
}
