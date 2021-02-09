{ stdenv, lib, fetchFromGitHub, sass }:

stdenv.mkDerivation rec {
  name = "phocus";

  src = fetchFromGitHub {
    owner = "fortuneteller2k";
    repo = "gtk";
    rev = "83d67b599031af6423ceef1fc09e2ecd8535a2c4";
    sha256 = "sha256-SDDCxzFFgUNxEbJbtflV7IkdPxPygtGx+mtgKjOiHqI=";
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
