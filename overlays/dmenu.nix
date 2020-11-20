self: super:
{
  dmenu = super.dmenu.overrideAttrs (old: {
    src = super.fetchFromGitHub {
      owner = "fortuneteller2k";
      repo = "dmenu";
      rev = "7a8c35cc845eff23baae89ccb25a04e98524e555";
      sha256 = "0ikjrif5x927gkqshkl0v5a386zkrrncch27z6n3xbjcl4c0lmx6";
    };
    patches = [];
  });
}
