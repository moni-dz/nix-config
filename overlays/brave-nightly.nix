final: prev: {
  brave-nightly = prev.brave.overrideAttrs (_: rec {
    __contentAddressed = true;

    version = "1.48.91";

    src = prev.fetchurl {
      url = "https://github.com/brave/brave-browser/releases/download/v${version}/brave-browser-nightly_${version}_amd64.deb";
      sha256 = "sha256-Lh8dhLJP125/Yh/2F0mDvwFN9ab6TqoKtJrPbk+diaQ=";
    };

    installPhase =
      let
        rpath = prev.lib.makeLibraryPath (with prev; with xorg; [
          alsa-lib
          at-spi2-atk
          at-spi2-core
          atk
          cairo
          cups
          dbus
          expat
          fontconfig
          freetype
          gdk-pixbuf
          glib
          gnome2.GConf
          gtk3
          libdrm
          libpulseaudio
          libX11
          libxkbcommon
          libXScrnSaver
          libXcomposite
          libXcursor
          libXdamage
          libXext
          libXfixes
          libXi
          libXrandr
          libXrender
          libxshmfence
          libXtst
          libuuid
          mesa
          nspr
          nss
          pango
          pipewire
          udev
          xdg-utils
          xorg.libxcb
          zlib
        ]);
      in
      ''
        runHook preInstall

        mkdir -p $out $out/bin

        cp -R usr/share $out
        cp -R opt/ $out/opt

        export BINARYWRAPPER=$out/opt/brave.com/brave-nightly/brave-browser-nightly

        # Fix path to bash in $BINARYWRAPPER
        substituteInPlace $BINARYWRAPPER \
          --replace /bin/bash ${prev.stdenv.shell}

        ln -sf $BINARYWRAPPER $out/bin/brave

        for exe in $out/opt/brave.com/brave-nightly/{brave,chrome_crashpad_handler}; do
        patchelf \
            --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
            --set-rpath "${rpath}" $exe
        done

        # Fix paths
        substituteInPlace $out/share/applications/brave-browser-nightly.desktop \
            --replace /usr/bin/brave-browser-nightly $out/bin/brave
        substituteInPlace $out/share/gnome-control-center/default-apps/brave-browser-nightly.xml \
            --replace /opt/brave.com $out/opt/brave.com
        substituteInPlace $out/share/menu/brave-browser-nightly.menu \
            --replace /opt/brave.com $out/opt/brave.com
        substituteInPlace $out/opt/brave.com/brave-nightly/default-app-block \
            --replace /opt/brave.com $out/opt/brave.com
      
        # Correct icons location
        icon_sizes=("16" "22" "24" "32" "48" "64" "128" "256")
      
        for icon in ''${icon_sizes[*]}
        do
            mkdir -p $out/share/icons/hicolor/$icon\x$icon/apps
            ln -s $out/opt/brave.com/brave-nightly/product_logo_$icon.png $out/share/icons/hicolor/$icon\x$icon/apps/brave-browser.png    
        done

        # Replace xdg-settings and xdg-mime
        ln -sf ${prev.xdg-utils}/bin/xdg-settings $out/opt/brave.com/brave-nightly/xdg-settings
        ln -sf ${prev.xdg-utils}/bin/xdg-mime $out/opt/brave.com/brave-nightly/xdg-mime
      
        runHook postInstall
      '';

    installCheckPhase = ''
      # Bypass upstream wrapper which suppresses errors
      $out/opt/brave.com/brave-nightly/brave --version
    '';
  });
}
