self: super:
{
  dmenu = super.dmenu.overrideAttrs (old: {
    src = super.fetchFromGitHub {
      owner = "fortuneteller2k";
      repo = "dmenu";
      rev = "b6c45ad9d7eb2156ace1f97e0fb71a9ce68976ba";
      sha256 = "0ygbwx1ky6gf9z01vinmypv8iksrbq7hlqxaphq1mg1n5g39nrx7";
    };
    patches = [];
  });
}
