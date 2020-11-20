self: super:
{
  papirus-icon-theme = super.papirus-icon-theme.overrideAttrs (old: {
    src = super.fetchFromGitHub {
      owner = "PapirusDevelopmentTeam";
      repo = "papirus-icon-theme";
      rev = "20201031";
      sha256 = "1skwjaa9gh78hg663ygdkj65xll58yvwpm6108rkw2mgagw36wzk";
   };
  });
}
