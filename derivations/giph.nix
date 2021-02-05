{ lib, stdenv, ffmpeg, slop, libnotify, xdotool }:

stdenv.mkDerivation rec {
  pname = "giph";
  version = "1.1.1";

  src = fetchFromGitHub {
    owner = "phisch";
    repo = "giph";
    rev = version;
    sha256 = lib.fakeSha256;
  };

  buildInputs = [ ffmpeg xdotool slop libnotify ];

  installFlags = [ "DESTDIR=$(out)" "PREFIX=" ];

  meta = with lib; {
    description = "Simple gif recorder";
    homepage = "https://github.com/phisch/giph";
    license = [ licenses.mit ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
