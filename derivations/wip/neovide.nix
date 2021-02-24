{ lib, rustPlatform, fetchFromGitHub, pkg-config }:

rustPlatform.buildRustPackage rec {
  pname = "neovide";
  version = "0.5.0";

  src = fetchFromGitHub {
    owner = "Kethku";
    repo = pname;
    rev = version;
    sha256 = lib.fakeSha256;
  };

  nativeBuildInputs = [ pkg-config ]
    }
