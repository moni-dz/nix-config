{ config, lib, pkgs, options, ... }:

let theme = import ../../config/theme.nix;
in
{
  boot = {
    kernelPackages = pkgs.kernel.linuxPackages_xanmod;

    kernelParams = [
      "rw"
      "mitigations=off"
      "acpi_backlight=vendor"
      "nmi_watchdog=0"
      "systemd.watchdog-device=/dev/watchdog"
      theme.colors.vt-red
      theme.colors.vt-grn
      theme.colors.vt-blu
    ];

    kernel.sysctl = {
      "vm.swappiness" = 10;
      "vm.vfs_cache_pressure" = 75;
      "kernel.printk" = "3 3 3 3";
      "kernel.unprivileged_userns_clone" = 1;
    };

    loader = {
      efi.canTouchEfiVariables = true;

      grub = {
        enable = true;
        device = "nodev";
        efiSupport = true;
        useOSProber = true;
        gfxmodeEfi = "1366x768";
      };
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  hardware = {
    bluetooth = {
      enable = true;
      hsphfpd.enable = true;
      package = pkgs.bluezFull;
    };

    cpu = {
      amd.updateMicrocode = true;
      intel.updateMicrocode = true;
    };

    enableAllFirmware = true;

    enableRedistributableFirmware = true;

    opengl = {
      enable = true;
      driSupport = true;
      extraPackages = with pkgs; [ vaapiVdpau libvdpau-va-gl ];
    };
  };

  imports = [
    ./hardware-configuration.nix
    ../../modules/services/xcompmgr.nix
    ../../modules/programs/river.nix
  ];

  i18n.defaultLocale = "en_US.UTF-8";

  environment = {
    binsh = "${pkgs.dash}/bin/dash";
    pathsToLink = [ "/share/zsh" ];

    sessionVariables = with pkgs; {
      LD_PRELOAD = "/etc/nixos/config/ld-preload-xcreatewindow.so";
      _JAVA_AWT_WM_NONREPARENTING = "1";
    };

    systemPackages = with pkgs; [
      alsaTools
      alsaUtils
      brightnessctl
      caffeine-ng
      coreutils
      curl
      dash
      envsubst
      fd
      file
      git
      glxinfo
      grim
      gnome3.nautilus
      gxmessage
      hacksaw
      haskell-language-server
      head.river
      hsetroot
      imagemagick
      jp2a
      jq
      libtool
      libva-utils
      notify-desktop
      ntfs3g
      pandoc
      pantheon.elementary-files
      pavucontrol
      pciutils
      psmisc
      pulseaudio
      ripgrep
      shellcheck
      shotgun
      unrar
      unzip
      util-linux
      wget
      xarchiver
      xclip
      xdo
      xdotool
      xidlehook
      xmonad-log
      xorg.xdpyinfo
      xorg.xsetroot
      xorg.xkill
      xorg.xwininfo
      xwallpaper
      zip
    ];
  };

  fonts = {
    fonts = with pkgs; [
      cozette
      curie
      emacs-all-the-icons-fonts
      etBook
      fantasque-sans-mono
      inter
      iosevka
      mplus-outline-fonts
      nerdfonts
      nur.repos.fortuneteller2k.iosevka-ft-bin
      sarasa-gothic
      scientifica
      symbola
      terminus_font
      twemoji-color-font
      xorg.fontbh100dpi
    ];

    fontconfig = {
      enable = true;
      dpi = 96;

      defaultFonts = {
        serif = [
          "Sarasa Gothic C"
          "Sarasa Gothic J"
          "Sarasa Gothic K"
        ];

        sansSerif = [
          "Sarasa Gothic C"
          "Sarasa Gothic J"
          "Sarasa Gothic K"
        ];

        monospace = [
          "Iosevka FT"
          "Iosevka Nerd Font"
          "Sarasa Mono C"
          "Sarasa Mono J"
          "Sarasa Mono K"
        ];

        emoji = [ "Twitter Color Emoji" ];
      };
    };
  };

  networking = {
    hostName = "superfluous";

    nameservers = [
      "1.1.1.1"
      "1.0.0.1"
      "8.8.8.8"
      "8.8.4.4"
    ];

    dhcpcd.enable = false;

    networkmanager = {
      enable = true;
      dns = "none";
    };

    useDHCP = false;

    interfaces = {
      eno1.useDHCP = true;
      wlo1.useDHCP = true;
    };
  };

  powerManagement.cpuFreqGovernor = "performance";

  programs = {
    bash = {
      promptInit = ''eval "$(${pkgs.starship}/bin/starship init bash)"'';
      interactiveShellInit = ''export HISTFILE=$HOME/.config/.bash_history'';
    };

    command-not-found.enable = false;
    dconf.enable = true;
    slock.enable = true;

    sway = {
      enable = true;
      wrapperFeatures.gtk = true;

      extraPackages = with pkgs; [
        swaylock
        swayidle
        swaybg
        wl-clipboard
        mako
        brightnessctl
        grim
        slurp
        sway-contrib.grimshot
        waybar
        wofi
      ];
    };

    qt5ct.enable = true;

    xss-lock = {
      enable = true;
      lockerCommand = "${config.security.wrapperDir}/slock";
    };
  };

  security = {
    protectKernelImage = true;
    rtkit.enable = true;
    sudo.wheelNeedsPassword = false;

    doas = {
      enable = true;
      wheelNeedsPassword = false;
    };
  };

  services = {
    auto-cpufreq.enable = true;
    blueman.enable = true;

    chrony = {
      enable = true;

      servers = [
        "ntp.pagasa.dost.gov.ph"
        "0.nixos.pool.ntp.org"
        "1.nixos.pool.ntp.org"
        "2.nixos.pool.ntp.org"
        "3.nixos.pool.ntp.org"
      ];
    };

    irqbalance.enable = true;
    journald.extraConfig = lib.mkForce "";

    openssh = {
      enable = true;
      gatewayPorts = "yes";
      permitRootLogin = "yes";
    };

    picom = {
      enable = true;
      refreshRate = 60;
      experimentalBackends = true;
      backend = "glx";
      vSync = true;

      settings = {
        blur = {
          method = "dual_kawase";
          strength = 7;
          background = false;
          background-frame = false;
          background-fixed = false;
        };

        blur-background-exclude = [
          "window_type = 'dock'"
          "window_type = 'desktop'"
          "_GTK_FRAME_EXTENTS@:c"
        ];

        daemon = true;
        use-damage = true;
      };
    };

    pipewire = {
      enable = true;
      socketActivation = false;

      alsa = {
        enable = true;
        support32Bit = true;
      };

      jack.enable = true;
      pulse.enable = true;

      config = {
        pipewire."context.properties" = {
          "link.max-buffers" = 16;
          "default.clock.min-quantum" = "32/48000";
          "default.clock.max-quantum" = "2048/48000";
        };

        pipewire-pulse = {
          "context.modules" = [
            { name = "libpipewire-module-protocol-native"; }
            { name = "libpipewire-module-client-node"; }
            { name = "libpipewire-module-adapter"; }
            { name = "libpipewire-module-metadata"; }

            {
              name = "libpipewire-module-rtkit";
              flags = [ "ifexists" "nofail" ];
            }

            {
              name = "libpipewire-module-protocol-pulse";
              args = {
                "server.address" = [ "unix:native" ];
                "pulse.min.req" = "32/48000";
                "pulse.min.quantum" = "32/48000";
                "pulse.min.frag" = "32/48000";
                "pulse.default.req" = "2048/48000";
                "pulse.default.frag" = "2048/48000";
              };
            }
          ];

          "stream.properties"."node.latency" = "32/48000";
        };
      };

      media-session.config.alsa-monitor.rules = [{
        # replace with your sink name
        matches = [{ "node.name" = "alsa_output.pci-0000_00_14.2.analog-stereo"; }];

        actions.update-props = {
          "audio.format" = "S16LE";
          "audio.rate" = 48000;
          "api.alsa.period-size" = 160;
        };
      }];
    };

    tlp.enable = true;
    upower.enable = true;

    udev.extraRules = ''
      KERNEL=="rtc0", GROUP="audio"
      KERNEL=="hpet", GROUP="audio"
    '';

    xserver = {
      enable = true;
      dpi = 96;

      displayManager = {
        sddm.enable = config.services.xserver.enable;

        lightdm = {
          enable = !config.services.xserver.enable;
          background = theme.wallpaper;

          greeters.gtk = {
            enable = true;

            theme = {
              name = "phocus";
              package = pkgs.phocus;
            };

            cursorTheme = {
              package = pkgs.vanilla-dmz;
              name = "${if theme.lightModeEnabled then "Vanilla-DMZ" else "Vanilla-DMZ-AA"}";
            };

            iconTheme = {
              package = pkgs.papirus-icon-theme;
              name = "${if theme.lightModeEnabled then "Papirus-Light" else "Papirus-Dark"}";
            };
          };
        };

        defaultSession = "none+xmonad";
      };

      extraConfig = ''
        Section "Device"
          Identifier "Radeon"
          Driver "radeon"
          Option "TearFree" "on"
        EndSection
        Section "Device"
          Identifier "AMD"
          Driver "amdgpu"
          Option "TearFree" "true"
        EndSection
      '';

      logFile = "/var/log/Xorg.0.log";
      useGlamor = true;

      windowManager = {
        "2bwm".enable = true;

        xmonad = with pkgs; {
          enable = true;
          config = import ../../config/xmonad.nix { inherit config pkgs theme; };
          extraPackages = hpkgs: with hpkgs; [ dbus xmonad-contrib ];

          ghcArgs = [
            "-O2"
            "-funfolding-use-threshold=16"
            "-fexcess-precision"
            "-optc-O3"
            "-optc-ffast-math"
          ];
        };
      };

      layout = "us";

      libinput = {
        enable = true;
        mouse.accelProfile = "flat";
        touchpad.naturalScrolling = true;
      };
    };
  };

  system = {
    userActivationScripts = {
      reloadWallpaper.text =
        if config.services.xserver.enable then
          "[ $DISPLAY ] && ${pkgs.xwallpaper}/bin/xwallpaper --zoom ${theme.wallpaper} || ${pkgs.coreutils}/bin/echo 'skipping...'"
        else
          "${pkgs.coreutils}/bin/echo 'skipping because on wayland...'";

      reloadXMonad.text =
        if config.services.xserver.enable then
          "[ $DISPLAY ] && ${pkgs.xmonad-with-packages}/bin/xmonad --restart || echo 'not in xmonad, skipping...' || ${pkgs.coreutils}/bin/echo 'skipping...'"
        else
          "${pkgs.coreutils}/bin/echo 'skipping because on wayland...'";
    };

    stateVersion = "20.09";
  };

  systemd = {
    extraConfig = "RebootWatchdogSec=5";

    services.rtkit-daemon.serviceConfig.ExecStart = [
      ""
      "${pkgs.rtkit}/libexec/rtkit-daemon --our-realtime-priority=95 --max-realtime-priority=90"
    ];

    user.services = {
      pipewire.wantedBy = [ "default.target" ];
      pipewire-pulse.wantedBy = [ "default.target" ];

      xidlehook = {
        description = "xidlehook daemon";
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];
        serviceConfig.ExecStart = lib.strings.concatStringsSep " "
          [
            ""
            "${pkgs.xidlehook}/bin/xidlehook"
            "--not-when-fullscreen"
            "--not-when-audio"
            "--timer 120 ${config.security.wrapperDir}/slock ''"
          ];
      };
    };
  };

  time.timeZone = "Asia/Manila";

  users.users.fortuneteller2k = {
    isNormalUser = true;
    home = "/home/fortuneteller2k";
    shell = pkgs.zsh;

    extraGroups = [
      "wheel"
      "networkmanager"
      "video"
      "audio"
      "realtime"
    ];
  };

  zramSwap = {
    enable = true;
    memoryPercent = 100;
  };
}
