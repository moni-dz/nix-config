{ lib, stdenv, requireFile, unzip }:

stdenv.mkDerivation {
  pname = "output-fonts";
  version = "2021-01-23";

  src = requireFile {
    name = "DJR-Fonts-2021-01-23-a40e1b7-Edu.zip";
    url = "https://djr.com/output/";
    sha256 = "d5a7b60d74b1b3d8997d08ca1e7b29fb54e9122ddddc469e3e1c2647883f7a80";
  };

  nativeBuildInputs = [ unzip ];

  phases = [ "unpackPhase" "installPhase" ];

  sourceRoot = ".";

  installPhase = ''
    mkdir -p $out/share/fonts/truetype
    mkdir -p $out/share/fonts/opentype
    find Output_Desktop -name "*.otf" -exec cp -a {} "$out"/share/fonts/opentype/ \;
    find Output_Desktop -name "*.ttf" -exec cp -a {} "$out"/share/fonts/truetype/ \;
    mkdir -p "$out"/share/doc
    cp -a *.txt "$out"/share/doc
  '';

  outputHashAlgo = "sha256";
  outputHashMode = "recursive";
  outputHash = "sha256-xxeMoEvp8mks5IY60PgpNlg/D7E64PJGsPMooZOojxo=";

  meta = with lib; {
    description = "Fonts for Code, from Font Bureau";
    longDescription = ''
      Output is a companion to my typeface Input, a type family designed for computer programming. 
      While Input is tuned to the specific demands of code,
      Output’s softer curves and moderate proportions confront the demands of readability in text and user interfaces.
    '';
    homepage = "https://djr.com/output/";
    license = licenses.unfree;
    maintainers = with maintainers; [ fortuneteller2k ];
    platforms = platforms.all;
  };
}
