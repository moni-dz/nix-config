{ lib, stdenv, fetchFromGitHub, sass }:

stdenv.mkDerivation rec {
  name = "phocus";

  src = fetchFromGitHub {
    owner = "fortuneteller2k";
    repo = "gtk";
    rev = "cf676e241a83c5392220ef95c9284f3fb657bfec";
    sha256 = "sha256-uICuOW7KVHGXGszOWjG8nYw9tpQ+tKe+bND861vYCS8=";
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
