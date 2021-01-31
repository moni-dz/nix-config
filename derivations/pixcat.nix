{ lib, python38Packages, buildPythonPackage, fetchPypi }:

buildPythonPackage rec {
  pname = "pixcat";
  version = "0.1.4";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-ZXyP4EUTyuzM1ghrNHqkuF22tMD3YbFiy5zXiavnq7Y=";
  };

  doCheck = false;

  propagatedBuildInputs = with python38Packages; [
    blessed
    docopt
    pillow
    requests
  ];

  meta = with lib; {
    description = "CLI and Python 3.6+ API to display images on a kitty terminal with optional resizing";
    homepage = "https://github.com/mirukana/pixcat";
    license = licenses.lgpl3Plus;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
