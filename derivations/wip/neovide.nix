{ lib, rustPlatform, fetchFromGitHub, cmake, curl, expat, freetype, openssl, pkg-config }:

rustPlatform.buildRustPackage rec {
  pname = "neovide";
  version = "0.5.0";

  src = fetchFromGitHub {
    owner = "Kethku";
    repo = pname;
    rev = version;
    sha256 = "sha256-NajO3mPPmdgQbYwS8wXKpLJVXsZ8gG71SQdWB4CGVPY=";
  };

  nativeBuildInputs = [ cmake pkg-config curl ];

  buildInputs = [
    openssl
    expat
    freetype
  ];

  cargoSha256 = "sha256-NQrslwqacXIx4jZRs6kAX2gkUyhrMZo+jFLr6z3Aj/Y=";

  meta = with lib; {
    description = "No Nonsense Neovim Client in Rust";
    homepage = "https://github.com/Kethku/neovide";
    maintainers = with maintainers; [ fortuneteller2k ];
    license = licenses.mit;
  };
}
