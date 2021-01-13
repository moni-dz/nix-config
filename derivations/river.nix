{ lib, stdenv, pkgs }:

stdenv.mkDerivation rec {
  name = "river";

  src = builtins.fetchGit {
    url = "https://github.com/ifreund/river";
    sha256 = lib.fakeSha256;
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
  installPhase = "zig build -Drelease-safe -Dxwayland --prefix $out install";

  nativeBuildInputs = with pkgs; [ pkgconfig ];

  meta = with lib; {
    description = "A dynamic tiling wayland compositor that takes inspiration from dwm and bspwm";
    homepage = "https://github.com/ifreund/river";
    license = licenses.mit;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
