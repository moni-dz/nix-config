with import <nixpkgs> {};

self: super:
{
  polybar = super.polybar.overrideAttrs (old: {
    src = super.fetchurl {
      url = "https://github.com/polybar/polybar/releases/download/3.5.0/polybar-3.5.0.tar";
      sha256 = "e8c1798c195854852fc7c99703287294123381101b371f171d4aa540aeb17afd";
    };

    buildInputs = [
      cairo
      xlibs.libXdmcp
      xorg.libpthreadstubs
      xorg.libxcb
      pcre
      python3
      xorg.xcbproto
      xorg.xcbutil
      xorg.xcbutilcursor
      xorg.xcbutilimage
      xorg.xcbutilrenderutil
      xorg.xcbutilwm
      xcbutilxrm
      libpulseaudio
    ];
  });
}
