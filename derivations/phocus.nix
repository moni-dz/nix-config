{ lib, stdenv, fetchFromGitHub, sass, theme }:

stdenv.mkDerivation rec {
  name = "phocus";

  src =
    let
      owner = "fortuneteller2k";
      repo = "gtk";
    in
    if theme.lightModeEnabled then
      fetchFromGitHub {
        inherit owner repo;
        rev = "9e015e98f31fb784010fa19d1c3ad9afa1344698";
        sha256 = "sha256-5fPXfYz0V2hO3B/6+VdAyCVCELNoZS00XTkZCV6JUM0=";
      } 
    else if theme.primaryColor == "red" then
      fetchFromGitHub {
        inherit owner repo;
        rev = "28a34466a9f8d90f4bbda88971c39b57495859be";
        sha256 = "sha256-jVgN3Z3B6gdHGLOFqGSTn54ApS7as0C/XfxpPl6hoMs=";
      }
    else
      fetchFromGitHub {
        inherit owner repo;
        rev = "1668dec0e87ac0ad9a591e9ca16c2356c31234a3";
        sha256 = "sha256-rUZe4SP4EPMpCmOcwkzV6cX7uRK+joP1VsWpQROlPAU=";
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
