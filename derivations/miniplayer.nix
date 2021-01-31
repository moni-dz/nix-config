{ lib, python38Packages, buildPythonPackage, fetchPypi, pixcat }:

buildPythonPackage rec {
  pname = "miniplayer";
  version = "1.0.1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-lDHbTDopDuMtkqGZDvvaUX+AuGwIrrtIUx8gJX/+wPY=";
  };

  doCheck = false;

  propagatedBuildInputs = with python38Packages; [
    mpd2
    ffmpeg-python
    pillow
    pixcat
  ];

  meta = with lib; {
    description =
      "A curses based mpd client with basic functionality and album art written for the Kitty terminal";
    homepage = "https://github.com/GuardKenzie/miniplayer";
    license = licenses.mit;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
