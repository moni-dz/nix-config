{ stdenv, lib, fetchFromGitHub, sass }:

stdenv.mkDerivation rec {
  name = "phocus";

  src = fetchFromGitHub {
    owner = "fortuneteller2k";
    repo = "gtk";
    rev = "49c8a1592e8f37cecfe89e330065bb06180d50ee";
    sha256 = "sha256-SEdrh6p1L5FGWk/RE+X48LPNNvLceLhZ5h/cxrC3xsU=";
  };

  nativeBuildInputs = [ sass ];

  installFlags = [ "DESTDIR=$(out)" "PREFIX=" ];

  meta = with lib; {
    description = "phocus with horizon theme";
    homepage = "https://github.com/fortuneteller2k/gtk";
    license = [ licenses.mit ];
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
