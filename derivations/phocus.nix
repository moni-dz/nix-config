{ lib, stdenv, fetchFromGitHub, sass, theme }:

stdenv.mkDerivation rec {
  name = "phocus";

  src = let
    owner = "fortuneteller2k";
    repo = "gtk";
  in if theme.lightModeEnabled then fetchFromGitHub {
    inherit owner repo;
    rev = "1e5a04b2e38daee4903973b0aed113d90074a03b";
    sha256 = "sha256-HVH8WQUuRs67qBlpHLX63e3AiVBQBhGbA8FEz4jrXTY=";
  } else fetchFromGitHub {
    inherit owner repo;
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
