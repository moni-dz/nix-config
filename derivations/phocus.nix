{ lib, stdenv, fetchFromGitHub, sass, theme }:

stdenv.mkDerivation rec {
  name = "phocus";

  src = fetchFromGitHub {
    owner = "fortuneteller2k";
    repo = "gtk";
    rev = "1ba31ebe9916505cfce7b98713f5fe805c5d39fb";
    sha256 = "sha256-hqgKht1Wq7yo3uMSqxuiIDqdSlx8XZMs7KfYy9WOLmQ=";
  };

  patchPhase = with theme.colors; ''
    substituteInPlace scss/gtk-3.0/_colors.scss \
      --replace 16161c ${bg} \
      --replace 232530 ${c0} \
      --replace 2e303e ${c8} \
      --replace e95678 ${c1} \
      --replace f09383 ${c11} \
      --replace fab795 ${c3} \
      --replace 29d398 ${c2} \
      --replace 1eb980 ${c10} \
      --replace 26bbd9 ${c4} \
      --replace ee64ae ${c5} \
      --replace ec6a88 ${c9} \
      --replace fdf0ed ${fg} \
      --replace aabbcc ${primary} \
      --replace ccbbaa ${c5}
  '';

  nativeBuildInputs = [ sass ];

  installFlags = [ "DESTDIR=$(out)" "PREFIX=" ];

  meta = with lib; {
    description = "phocus with horizon theme";
    homepage = "https://github.com/fortuneteller2k/gtk";
    license = [ licenses.mit ];
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
