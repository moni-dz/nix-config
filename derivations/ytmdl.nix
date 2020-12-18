{ pkgs, lib, ... }:

with pkgs.python3Packages;

buildPythonPackage rec {
  pname = "ytmdl";
  version = "2020.11.20.post1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-vMlMZo8AVX1wjgRL48Ty+/GlWhF0jpySduXt2vNRk9w=";
  };

  preCheck = ''
    mkdir tmp
    HOME=$PWD/tmp
  '';

  meta = with lib; {
    inherit version;
    description = "A simple app to get songs from YouTube in mp3 format with artist name, album name etc from sources like iTunes, LastFM, Deezer, Gaana etc.";
    homepage = "https://github.com/deepjyoti30/ytmdl";
    license = licenses.mit;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
