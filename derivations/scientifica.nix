{ lib, stdenv, fetchzip }:

stdenv.mkDerivation rec {
  pname = "scientifica";
  version = "v2.1";
  
  src = fetchzip {
    url = "https://github.com/NerdyPepper/scientifica/releases/latest/download/${pname}-${version}.tar";
    sha256 = "1mji70h5qdplx0rlhijrdpbmvd0c6fvnr70sla032gfs5g6f78cn";
  };

  dontBuild = true;
  installPhase = ''
    mkdir -p $out/share/fonts/truetype/
    mkdir -p $out/share/fonts/misc/
    cp ttf/* -d $out/share/fonts/truetype/
    cp otb/* -d $out/share/fonts/misc/
    cp bdf/* -d $out/share/fonts/misc/
  '';

  meta = with lib; {
    description = "Tall and condensed bitmap font for geeks";
    homepage = "https://github.com/NerdyPepper/scientifica";
    license = licenses.ofl;
    platforms = platforms.all;
  };
}
