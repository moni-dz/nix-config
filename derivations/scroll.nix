{ lib, stdenv }:

stdenv.mkDerivation rec {
  pname = "scroll";
  version = "0.1";

  src = fetchurl {
    url = "https://dl.suckless.org/tools/${pname}-${version}.tar.gz";
    sha256 = lib.fakeSha256;
  };

  installFlags = [ "PREFIX=$out" ];

  meta = with lib; {
    description = "Scrollback buffer program for st";
    homepage = "https://tools.suckless.org/scroll/";
    license = [ licenses.mit ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
