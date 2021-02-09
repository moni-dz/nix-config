{ lib, stdenv, fetchFromGitHub, pkgs }:

stdenv.mkDerivation rec {
  name = "ncmpcpp-artwork";

  src = fetchFromGitHub {
    owner = "evanc577";
    repo = "ncmpcpp";
    rev = "artwork-mpd";
    sha256 = "sha256-qyJsDLUCe7epVXWmqkZH/UOCafWdp6zuoMDRYyfyQ6Q=";
  };

  configureFlags = [
    "BOOST_LIB_SUFFIX="
    "--enable-outputs"
    "--enable-visualizer"
    "--with-fftw"
    "--with-taglib"
    "--enable-artwork"
  ];

  nativeBuildInputs = with pkgs; [
    autoconf
    automake
    libtool
    pkg-config
  ];

  buildInputs = with pkgs; [
    boost
    mpd_clientlib
    ncurses
    readline
    libiconv
    icu
    curl
    fftw
    taglib
    ueberzug
  ];

  preBuild = ''
    ./autogen.sh
    ./configure
  '';

  installFlags = [ "DESTDIR=$(out)" "PREFIX=/bin" ];

  meta = with lib; {
    description = "A featureful ncurses based MPD client inspired by ncmpc (with artwork support)";
    homepage = "https://rybczak.net/ncmpcpp/";
    changelog = "https://github.com/ncmpcpp/ncmpcpp/blob/${version}/CHANGELOG.md";
    license = licenses.gpl2Plus;
    maintainers = with maintainers; [ fortuneteller2k ];
    platforms = platforms.all;
  };
}
