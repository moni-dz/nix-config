{ lib, stdenv, fetchFromGitHub, sass, theme }:

stdenv.mkDerivation rec {
  name = "phocus";

  src =
    let
      owner = "fortuneteller2k";
      repo = "gtk";
    in
    if theme.lightModeEnabled then
      fetchFromGitHub
        {
          inherit owner repo;
          rev = "9e015e98f31fb784010fa19d1c3ad9afa1344698";
          sha256 = "sha256-5fPXfYz0V2hO3B/6+VdAyCVCELNoZS00XTkZCV6JUM0=";
        } else
      fetchFromGitHub {
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
