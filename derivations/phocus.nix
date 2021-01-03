{ stdenv, lib, fetchFromGitHub, sass }:

stdenv.mkDerivation rec {
  pname = "phocus";
  version = "master";

  src = fetchFromGitHub {
    owner = "fortuneteller2k";
    repo = "gtk";
    rev = version;
    sha256 = "sha256-jyiABd9ZTL1bWIYzFJ+NtGrQKLU3g3yiOr0PWWLII5k=";
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
