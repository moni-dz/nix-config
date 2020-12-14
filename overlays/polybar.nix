{ cairo, cmake, fetchFromGitHub, libXdmcp, libpthreadstubs, libxcb, pcre, pkgconfig
, python3, stdenv, xcbproto, xcbutil, xcbutilcursor, xcbutilimage
, xcbutilrenderutil, xcbutilwm, xcbutilxrm, libpulseaudio }:

final: prev:
{
  polybar = prev.polybar.overrideAttrs (old: {
    src = prev.fetchurl {
      url = "https://github.com/polybar/polybar/releases/download/3.5.0/polybar-3.5.0.tar";
      sha256 = "e8c1798c195854852fc7c99703287294123381101b371f171d4aa540aeb17afd";
    };

    buildInputs = [
      cairo
      libXdmcp
      libpthreadstubs
      libxcb
      pcre
      python3
      xcbproto
      xcbutil
      xcbutilcursor
      xcbutilimage
      xcbutilrenderutil
      xcbutilwm
      xcbutilxrm
      libpulseaudio
    ];
  });
}
