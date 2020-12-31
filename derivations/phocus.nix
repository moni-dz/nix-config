{ stdenv, lib, fetchFromGitHub, sass }:

stdenv.mkDerivation rec {
  pname = "phocus";
  version = "master";

  src = fetchFromGitHub {
    owner = "elkowar";
    repo = "gtk";
    rev = version;
    sha256 = "sha256-igKcx0bNFjbquAXxvbTG6qe9Ic/ccJeIq9tdNblpOhw=";
  };

  nativeBuildInputs = [ sass ];

  installFlags = [ "DESTDIR=$(out)" "PREFIX=" ];

  meta = with lib; {
    description = "phocus with horizon theme";
    homepage = "https://github.com/fortuneteller2k/phocus";
    license = [ licenses.mit ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
