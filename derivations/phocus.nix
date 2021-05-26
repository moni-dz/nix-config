{ lib, stdenvNoCC, fetchFromGitHub, sass, theme, src }:

stdenvNoCC.mkDerivation rec {
  pname = "phocus";
  version = "unstable-2021-05-05";

  inherit src;

  patchPhase = with theme.colors; ''
    runHook prePatch

    substituteInPlace scss/gtk-3.0/_colors.scss \
      --replace 16161c ${if theme.lightModeEnabled then fg else bg} \
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
      --replace fdf0ed ${if theme.lightModeEnabled then bg else fg} \
      --replace aabbcc ${primary} \
      --replace ccbbaa ${secondary}

    runHook postPatch
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
