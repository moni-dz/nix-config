{ lib, buildPythonPackage, fetchPypi, blessed }:

buildPythonPackage rec {
  pname = "dashing";
  version = "0.1.0";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-JRRgjg8pp3Xb0bERFWEhnOg9U8+kuqL+QQH6uE/Vbxs=";
  };

  propagatedBuildInputs = [ blessed ];

  meta = with lib; {
    homepage = "https://github.com/FedericoCeratto/dashing";
    description = "Terminal dashboards for Python";
    license = licenses.lgpl3Only;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
