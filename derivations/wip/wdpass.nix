{ lib, python38Packages, buildPythonPackage, fetchPypi }:

buildPythonPackage rec {
  pname = "wdpass";
  version = "0.0.3";

  src = fetchPypi {
    inherit pname version;
    sha256 = lib.fakeSha256;
  };

  doCheck = false;

  propagatedBuildInputs = with python38Packages; [

  ];

  meta = with lib; {
    description = "WD Passport Ultra Linux Utilities (using Python 3)";
    homepage = "https://github.com/7aman/wdpass";
    license = license.mit;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
