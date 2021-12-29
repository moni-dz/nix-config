{ lib, stdenvNoCC, fetchFromGitHub, sass, src }:

let
  colors = {
    base00 = "1C1E26";
    base01 = "232530";
    base02 = "2E303E";
    base03 = "6F6F70";
    base04 = "9DA0A2";
    base05 = "CBCED0";
    base06 = "DCDFE4";
    base07 = "E3E6EE";
    base08 = "E93C58";
    base09 = "E58D7D";
    base0A = "EFB993";
    base0B = "EFAF8E";
    base0C = "24A8B4";
    base0D = "DF5273";
    base0E = "B072D1";
    base0F = "E4A382";
  };
in
stdenvNoCC.mkDerivation rec {
  pname = "phocus";
  version = "unstable-2021-05-05";

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
