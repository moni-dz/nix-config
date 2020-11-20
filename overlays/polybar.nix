with import <nixpkgs> {};

self: super:
{
  polybar = super.polybar.overrideAttrs (old: {
    src = super.fetchFromGitHub {
      owner = "polybar";
      repo = "polybar";
      rev = "14a948d75d2fe6dae4d95afa06f18dbdfb147243";
      sha256 = "0pa9hps1zwbf567gjd9wjax8qnamyhl6naam04s2hz1apk7pci5b";
      fetchSubmodules = true;
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
