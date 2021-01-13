{ stdenv, lib }:

stdenv.mkDerivation rec {
  pname = "scroll";
  version = "master";

  src = ./tarballs/scroll.tar.gz;

  installPhase = "make install PREFIX=$out";

  meta = with lib; {
    description = "scrollbackbuffer program for st";
    homepage = "https://git.suckless.org/scroll/";
    license = [ licenses.mit ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
