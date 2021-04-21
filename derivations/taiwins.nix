{ lib
, stdenv
, fetchFromGitHub
, meson
, ninja
, pkg-config
, wayland
, cairo
, fontconfig
, freetype
, libdrm
, libinput
, libGL
, librsvg
, libX11
, libxcb
, libxkbcommon
, linux-pam
, lua5_3
, mesa
, wayland-protocols
, xwayland
, pixman
}:

stdenv.mkDerivation rec {
  pname = "taiwins";
  version = "unstable-2021-04-21";

  src = fetchFromGitHub {
    owner = pname;
    repo = pname;
    rev = "1ce60a07f0e3fc0e10051e9ab2e49bab261c1e28";
    sha256 = "sha256-IKES9nQESgsryVY57EIeX+7QbA6rwX5FsGPEQ45MqUg=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ meson ninja pkg-config lua5_3 wayland ];

  buildInputs = [
    cairo
    fontconfig
    freetype
    libdrm
    libinput
    libGL
    librsvg
    libX11
    libxcb
    libxkbcommon
    linux-pam
    lua5_3
    mesa
    wayland-protocols
    xwayland
    pixman
  ];
}
