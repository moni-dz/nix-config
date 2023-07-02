{ lib, python3, fetchFromGitHub }:

python3.pkgs.buildPythonApplication rec {
  pname = "asitop";
  version = "74ebe2c";

  src = fetchFromGitHub {
    owner = "tlkh";
    repo = pname;
    rev = "74ebe2cbc23d5b1eec874aebb1b9bacfe0e670cd";
    hash = "sha256-SQTIV+OvFSH3TNIFP8ts+nnZocaEg+3yOKDtzlv1q2A=";
  };

  doCheck = false;

  propagatedBuildInputs = with python3.pkgs; [ psutil pydashing ];

  meta = with lib; {
    homepage = "https://github.com/tikh/asitop";
    description = "Perf monitoring CLI tool for Apple Silicon";
    license = licenses.mit;
    platforms = [ "aarch64-darwin" ];
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
