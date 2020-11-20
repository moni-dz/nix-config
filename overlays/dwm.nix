self: super:
{
  dwm = super.dwm.overrideAttrs (old: {
    src = super.fetchFromGitHub {
      owner = "fortuneteller2k";
      repo = "dwm";
      rev = "8b741ea695f5cec27d2b0a8494817ade4f0c8d1d";
      sha256 = "1dlmvw6399g7gqhr2g4dv7a4r8jwq0a6prm29v5r919240dkpndf";
    };
    patches = [];
  });
}
