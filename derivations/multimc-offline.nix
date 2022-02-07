{ lib, mkDerivation, makeDesktopItem, fetchFromGitHub, cmake, jdk8, jdk, zlib, file, makeWrapper, xorg, libpulseaudio, qtbase, libGL, msaClientID ? "" }:

let
  libpath = with xorg; lib.makeLibraryPath [ libX11 libXext libXcursor libXrandr libXxf86vm libpulseaudio libGL ];
in
mkDerivation rec {
  pname = "multimc-offline";
  version = "0.6.14";

  src = fetchFromGitHub {
    owner = "AfoninZ";
    repo = "MultiMC5-Cracked";
    rev = "4afe2466fd5639bf8a03bfb866c070e705420d86";
    hash = "sha256-CLRXatiNbPv57LwT8fbOAlcRjMNISeaM5hLgL1ARF8Q=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake file makeWrapper ];
  buildInputs = [ qtbase jdk8 zlib ];

  patches = [ ./patches/0001-pick-latest-java-first.patch ];

  postPatch = ''
    # hardcode jdk paths
    substituteInPlace launcher/java/JavaUtils.cpp \
      --replace 'scanJavaDir("/usr/lib/jvm")' 'javas.append("${jdk}/lib/openjdk/bin/java")' \
      --replace 'scanJavaDir("/usr/lib32/jvm")' 'javas.append("${jdk8}/lib/openjdk/bin/java")'
    # add client ID
    substituteInPlace notsecrets/Secrets.cpp \
      --replace 'QString MSAClientID = "";' 'QString MSAClientID = "${msaClientID}";'
  '';

  cmakeFlags = [ "-DLauncher_LAYOUT=lin-nodeps" ];

  preFixup = ''
    mkdir -p $out/lib
    mv $out/bin/*.so $out/lib/
  '';

  postInstall = ''
    # xorg.xrandr needed for LWJGL [2.9.2, 3) https://github.com/LWJGL/lwjgl/issues/128
    wrapProgram $out/bin/UltimMC \
      --set GAME_LIBRARY_PATH /run/opengl-driver/lib:${libpath} \
      --prefix PATH : ${lib.makeBinPath [ xorg.xrandr ]} \
      --add-flags '-d "''${XDG_DATA_HOME-$HOME/.local/share}/UltimMC"'
  '';
}
