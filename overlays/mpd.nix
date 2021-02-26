final: prev: {
  mpd = prev.mpd.overrideAttrs (old: {
    src = fetchFromGitHub {
      owner  = "MusicPlayerDaemon";
      repo   = "MPD";
      rev    = "v0.22.6";
      sha256 = "sha256-CKNw3K/z5UrTIp9ryWq7UaTz768AigaoCIcoJ4iW1j4=";
    };
  });
}
