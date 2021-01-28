{ rustPlatform, lib, fetchFromGitHub, pkgs, rust }:

rustPlatform.buildRustPackage rec {
  pname = "eww";
  version = "master";

  src = fetchFromGitHub {
    owner = "elkowar";
    repo = pname;
    rev = version;
    sha256 = "sha256-diM174QlYAmhoK6NTXKDbkn6n6Vw3e8izwsMD256AJo=";
  };

  nativeBuildInputs = [ rust pkgs.pkg-config ];
  buildInputs = with pkgs; [ 
    gtk3
    cairo
    glib
    atk
    pango
    gdk-pixbuf
    gdk-pixbuf-xlib
  ];
  
  checkPhase = null;
  cargoSha256 = "sha256-I/oEDh2anNyI9qm9bpkgcWAKV3rNgob4nJNZskVLnk4=";

  meta = with lib; {
    description =
      "A standalone widget system made in Rust to add AwesomeWM like widgets to any WM";
    homepage = "https://github.com/elkowar/eww";
    licenses = licenses.mit;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
