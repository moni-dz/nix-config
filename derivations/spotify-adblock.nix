{ lib, fetchurl }:

let
  version = "1.4";
in
fetchurl rec {
  name = "spotify-adblock-${version}";

  url = "https://github.com/abba23/spotify-adblock-linux/releases/download/v${version}/spotify-adblock.so";

  sha256 = "sha256-MOjQGCaBQcXT/x9m7tO0oBjtpEumHaypGAX/wFt+H/w=";

  downloadToTemp = true;

  recursiveHash = true;

  postFetch = ''
    mkdir -p $out/lib
    mv $downloadedFile $out/lib/spotify-adblock.so
  '';

  meta = with lib; {
    homepage = "https://github.com/abba23/spotify-adblock-linux";
    description = "Spotify adblocker for linux";
    license = licenses.gpl3Only;
    platforms = platforms.linux;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
