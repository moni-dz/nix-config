{ rustPlatform, lib, fetchFromGitHub, pkgs, rust }:

rustPlatform.buildRustPackage rec {
  pname = "eww";
  version = "master";

  src = fetchFromGitHub {
    owner = "elkowar";
    repo = pname;
    rev = version;
    sha256 = "sha256-okA97zqP/IUrc4YVwQBt2jBvvC+W2mBd5wZfcFVRMbA=";
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
  cargoSha256 = "sha256-23FBMJ0E9laHzsTg6n24k4o61jBrjvLXF3zPvoCvHpU=";

  meta = with lib; {
    description =
      "A standalone widget system made in Rust to add AwesomeWM like widgets to any WM";
    homepage = "https://github.com/elkowar/eww";
    license = licenses.mit;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
