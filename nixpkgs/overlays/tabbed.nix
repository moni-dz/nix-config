self: super:
{
  tabbed = super.tabbed.overrideAttrs (old: {
    src = super.fetchFromGitHub {
      owner = "fortuneteller2k";
      repo = "tabbed";
      rev = "8537d3f4228dbfa9d5b7166e652c913180605b09";
      sha256 = "1vzk9vrjr8dbj3c9zhwqk62890gsnmm7ryqfflhhrxb3ardkj2w0";
    };
    patches = [];
  });
}
