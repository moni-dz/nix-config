{ rustPlatform, lib, fetchFromGitHub, glib }:

let
  rpathLibs = [ glib ];
in rustPlatform.buildRustPackage rec {
  pname = "eww";
  version = "master";

  src = fetchFromGitHub {
    owner = "elkowar";
    repo = pname;
    rev = version;
    sha256 = "sha256-9FUBSojGlJHpFXTPNjusTQ7DJ9pl6U10C3s78blN4Hc=";
  };

  RUSTC_BOOTSTRAP = 1;

  doCheck = false;

  cargoSha256 = "sha256-bE8UZuYW2OwwJNTqjWt4vjTuL7RNHps052qjOQhwf48=";

  nativeBuildInputs = rpathLibs;
  buildInputs = rpathLibs;

  meta = with lib; {
    description = "A standalone widget system made in Rust to add AwesomeWM like widgets to any WM";
    homepage = "https://github.com/elkowar/eww";
    licenses = licenses.mit;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
