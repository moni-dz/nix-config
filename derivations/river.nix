{ lib, stdenv, pkgs }:

stdenv.mkDerivation rec {
  name = "river";

  src = pkgs.fetchgit {
    url = "https://github.com/ifreund/river";
    sha256 = "sha256-aVx7o6mjZ5oRKn0KWUCJAismRv8Bu1CdIbQofgja8rc=";
    fetchSubmodules = true;
  };

  buildInputs = with pkgs; [
    zig
    wayland
    wayland-protocols
    wlroots
    libxkbcommon
    libudev
    libevdev
    xorg.libX11
    pixman
    libGL
  ];

  preBuild = "export HOME=$TMPDIR;";
  installPhase = "zig build -Drelease-safe --prefix $out install";

  nativeBuildInputs = with pkgs; [ pkgconfig ];

  meta = with lib; {
    description = "A dynamic tiling wayland compositor that takes inspiration from dwm and bspwm";
    homepage = "https://github.com/ifreund/river";
    license = licenses.mit;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
