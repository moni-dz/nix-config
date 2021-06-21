{ lib
, stdenv
, fetchFromGitHub
, meson
, ninja
, pkg-config
, wayland
, cairo
, dbus
, fontconfig
, freetype
, libdrm
, libinput
, libGL
, libGLU
, librsvg
, libX11
, libxcb
, libxkbcommon
, linux-pam
, lua5_3
, mesa
, wayland-protocols
, xwayland
, pam
, pixman
}:

stdenv.mkDerivation rec {
  pname = "taiwins";
  version = "unstable-2021-06-07";

  src = fetchFromGitHub {
    owner = pname;
    repo = pname;
    rev = "6b669f8770fefb1da5114471b02c54e3dcb69539";
    sha256 = "sha256-ANzQkKEf2/tBuMBl10WWlMGIwa84C+GR8mihqqpLbO4=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ meson ninja pkg-config lua5_3 wayland ];

  buildInputs = [
    cairo
    dbus
    fontconfig
    freetype
    libdrm
    libinput
    libGL
    libGLU
    librsvg
    libX11
    libxcb
    libxkbcommon
    linux-pam
    lua5_3
    mesa
    wayland-protocols
    xwayland
    pam
    pixman
  ];
}
