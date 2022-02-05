{ lib, stdenvNoCC, fetchFromGitHub, sass, src }:

let
  colors = {
    base00 = "212121";
    base01 = "303030";
    base02 = "353535";
    base03 = "4A4A4A";
    base04 = "B2CCD6";
    base05 = "EEFFFF";
    base06 = "EEFFFF";
    base07 = "FFFFFF";
    base08 = "F07178";
    base09 = "F78C6C";
    base0A = "FFCB6B";
    base0B = "C3E88D";
    base0C = "89DDFF";
    base0D = "82AAFF";
    base0E = "C792EA";
    base0F = "FF5370";
  };
in
stdenvNoCC.mkDerivation rec {
  pname = "phocus";
  version = "unstable-2021-05-05";

  __contentAddressed = true;

  inherit src;

  patchPhase = with colors; ''
    runHook prePatch

    substituteInPlace scss/gtk-3.0/_colors.scss \
      --replace 16161c ${base01} \
      --replace 232530 ${base00} \
      --replace 2e303e ${base02} \
      --replace e95678 ${base08} \
      --replace f09383 ${base0A} \
      --replace fab795 ${base0A} \
      --replace 29d398 ${base0B} \
      --replace 1eb980 ${base0B} \
      --replace 26bbd9 ${base0D} \
      --replace ee64ae ${base0E} \
      --replace ec6a88 ${base08} \
      --replace fdf0ed ${base05} \
      --replace aabbcc ${base0B} \
      --replace ccbbaa ${base08}

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
