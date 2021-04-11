{ config, pkgs, options, ... }:

let theme = import ../../config/theme.nix;
in
{
  boot = {
    kernelPackages = pkgs.head.linuxPackages_xanmod;

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
      "vm.swappiness" = 1;
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

    plymouth.enable = true;
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

  imports = [ ./hardware-configuration.nix ];

  i18n.defaultLocale = "en_US.UTF-8";

  environment = {
    pathsToLink = [ "/share/zsh" ];

    sessionVariables = with pkgs; {
      LD_PRELOAD = "/etc/nixos/config/ld-preload-xcreatewindow.so";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      XSECURELOCK_BLANK_TIMEOUT = "5";
      XSECURELOCK_BLANK_DPMS_STATE = "suspend";
      XSECURELOCK_KEY_XF86AudioPlay_COMMAND = "${playerctl}/bin/playerctl play-pause";
      XSECURELOCK_KEY_XF86AudioPrev_COMMAND = "${playerctl}/bin/playerctl previous";
      XSECURELOCK_KEY_XF86AudioNext_COMMAND = "${playerctl}/bin/playerctl next";
      XSECURELOCK_KEY_XF86AudioMute_COMMAND = "/home/fortuneteller2k/.local/bin/volume toggle";
      XSECURELOCK_KEY_XF86AudioRaiseVolume_COMMAND = "/home/fortuneteller2k/.local/bin/volume up";
      XSECURELOCK_KEY_XF86AudioLowerVolume_COMMAND = "/home/fortuneteller2k/.local/bin/volume down";
      XSECURELOCK_KEY_XF86MonBrightnessUp_COMMAND = "${brightnessctl}/bin/brightnessctl s +10%";
      XSECURELOCK_KEY_XF86MonBrightnessDown_COMMAND = "${brightnessctl}/bin/brightnessctl s 10%-";
    };

    systemPackages = with pkgs; [
      alsaTools
      alsaUtils
      brightnessctl
      coreutils
      curl
      dash
      envsubst
      fd
      file
      git
      glxinfo
      gxmessage
      hacksaw
      haskell-language-server
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
      pcmanfm
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
      xsecurelock
      xwallpaper
      zip
    ];
  };

  fonts = {
    fonts = with pkgs; [
      cozette
      emacs-all-the-icons-fonts
      fantasque-sans-mono
      inter
      iosevka-ft
      mplus-outline-fonts
      nerdfonts
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
    qt5ct.enable = true;

    xss-lock = {
      enable = true;
      lockerCommand = "${pkgs.xsecurelock}/bin/xsecurelock";
    };
  };
  security = {
    rtkit.enable = true;
    sudo.wheelNeedsPassword = false;

    doas = {
      enable = true;
      wheelNeedsPassword = false;
    };
  };

  services = {
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

    dbus.packages = with pkgs; [ gnome3.dconf ];
    irqbalance.enable = true;

    openssh = {
      enable = true;
      gatewayPorts = "yes";
      permitRootLogin = "yes";
    };

    pipewire = {
      enable = true;
      socketActivation = false;

      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };

    tlp.enable = true;
    upower.enable = true;

    xserver = {
      enable = true;
      dpi = 96;

      displayManager = {
        lightdm = {
          enable = true;
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
      videoDrivers = [ "radeon" ];

      windowManager = {
        "2bwm".enable = true;

        xmonad = {
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

          haskellPackages =
            pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
              xmonad = pkgs.fetchFromGitHub {
                owner = "xmonad";
                repo = "xmonad";
                rev = "46f637e0bed18fa09e46e8f8ad5ccd0ae19d6fa0";
                sha256 = "sha256-oCwxyxMbo/LEbQQlw0LnopMnLSysarV/HMcpeK3mVgY=";
              };
              xmonad-contrib = pkgs.fetchFromGitHub {
                owner = "xmonad";
                repo = "xmonad-contrib";
                rev = "0ebd3a0534f1b4cdb0aa931bf16b296e557dd811";
                sha256 = "sha256-v36LYi7muTz/6u2Y7Kdkv73TeM0LmqKhO313Cyvb2jg=";
              };
            });
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
      reloadWallpaper.text = "[ $DISPLAY ] && ${pkgs.xwallpaper}/bin/xwallpaper --zoom ${theme.wallpaper} || ${pkgs.coreutils}/bin/echo 'skipping...'";
      reloadXMonad.text = "[ $DISPLAY ] && ${pkgs.xmonad-with-packages}/bin/xmonad --restart || echo 'not in xmonad, skipping...' || ${pkgs.coreutils}/bin/echo 'skipping...'";
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
