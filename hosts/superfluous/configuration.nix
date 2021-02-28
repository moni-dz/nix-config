{ config, pkgs, options, ... }:

let
  theme = (import ../../config/theme.nix);
in
{
  boot = {
    kernelPackages = pkgs.fork.linuxPackages_xanmod;
    kernelParams = [
      "rw"
      "mitigations=off"
      "acpi_backlight=vendor"
      "nmi_watchdog=0"
      "systemd.watchdog-device=/dev/watchdog"
      "vt.default_red=0x16,0xe9,0x29,0xfa,0x26,0xee,0x59,0xfd,0x23,0xec,0x3f,0xfb,0x3f,0xf0,0x6b,0xfd"
      "vt.default_grn=0x16,0x56,0xd3,0xb7,0xbb,0x64,0xe3,0xf0,0x35,0x6a,0xda,0xc3,0xc6,0x75,0xe6,0xf0"
      "vt.default_blu=0x1c,0x78,0x98,0x95,0xd9,0xae,0xe3,0xed,0x30,0x88,0xa4,0xa7,0xde,0xb7,0xe6,0xed"
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
    cpu.amd.updateMicrocode = true;
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
    systemPackages = with pkgs; [
      alsaTools
      alsaUtils
      brightnessctl
      ccls
      cmake
      copyq
      coreutils
      curl
      dash
      dragon-drop
      elixir
      envsubst
      eww
      fd
      file
      ffmpeg
      font-manager
      gcc
      ghc
      git
      gnumake
      go
      gxmessage
      hacksaw
      haskell-language-server
      hsetroot
      imagemagick
      jp2a
      jq
      libsForQt514.qtstyleplugins
      libtool
      libva-utils
      nixfmt
      nixpkgs-review
      nodePackages.npm
      nodePackages.typescript
      nodejs
      notify-desktop
      ntfs3g
      pandoc
      pavucontrol
      pciutils
      pcmanfm
      pulseaudio
      python3
      python39Packages.grip
      ripgrep
      rust-analyzer-unwrapped
      rustup
      shellcheck
      shotgun
      slock
      stack
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
      zig
      zip
    ];
  };
  fonts = {
    fonts = with pkgs; [
      cozette
      emacs-all-the-icons-fonts
      fantasque-sans-mono
      input-fonts
      inter
      iosevka-ft
      mplus-outline-fonts
      nerdfonts
      output-fonts
      sarasa-gothic
      scientifica
      symbola
      twemoji-color-font
      xorg.fontbh100dpi
    ];
    fontconfig = {
      enable = true;
      dpi = 96;
      defaultFonts = {
        serif = [ "Sarasa Gothic J" ];
        sansSerif = [ "Sarasa Gothic J" ];
        monospace = [
          "Iosevka FT"
          "Iosevka Nerd Font"
          "Sarasa Mono J"
        ];
        emoji = [ "Twitter Color Emoji" ];
      };
    };
  };
  networking = {
    hostName = "superfluous";
    nameservers = [ "1.1.1.1" "1.0.0.1" ];
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
    slock.enable = true;
    xss-lock = {
      enable = true;
      lockerCommand = "${pkgs.slock}/bin/slock";
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
    picom = {
      enable = true;
      inactiveOpacity = 0.9;
      refreshRate = 60;
      experimentalBackends = true;
      backend = "glx";
      vSync = true;
      fade = true;
      fadeDelta = 1;
      settings = {
        blur = {
          method = "dual_kawase";
          strength = 5;
          background = false;
          background-frame = false;
          background-fixed = false;
        };
        blur-background-exclude = [
          "window_type = 'dock'"
          "window_type = 'desktop'"
          "_GTK_FRAME_EXTENTS@:c"
        ];
        use-ewmh-active-win = true;
        corner-radius = 8;
        round-borders = 1;
        rounded-corners-exclude = [
          "window_type = 'dock'"
          "window_type = 'desktop'"
          "!name ~= ''" # Exclude any "Unknown" windows  
        ];
        transition-length = 200;
      };
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
      config = (import ../../config/xorg-amd-tearfree.nix);
      displayManager = {
        lightdm = {
          enable = true;
          background = ../../config/wallpapers/horizon.jpg;
          greeters.gtk = {
            enable = true;
            cursorTheme = {
              name = "DMZ ${if theme.lightModeEnabled then "(Black)" else "(White)"}";
              package = pkgs.vanilla-dmz;
            };
            iconTheme = {
              name = "Papirus";
              package = pkgs.papirus-icon-theme;
            };
            theme = {
              name = "phocus";
              package = pkgs.phocus;
            };
          };
        };
        defaultSession = "none+xmonad";
      };
      windowManager = {
        xmonad = {
          enable = true;
          config = (import ../../config/xmonad.nix {
            inherit pkgs theme;
          });
          extraPackages = hpkgs: with hpkgs; [ dbus xmonad-contrib ];
          haskellPackages = pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
            xmonad = pkgs.fetchFromGitHub {
              owner = "xmonad";
              repo = "xmonad";
              rev = "a90558c07e3108ec2304cac40e5d66f74f52b803";
              sha256 = "sha256-+TDKhCVvxoRLzHZGzFnClFqKcr4tUrwFY1at3Rwllus=";
            };
            xmonad-contrib = pkgs.fetchFromGitHub {
              owner = "xmonad";
              repo = "xmonad-contrib";
              rev = "cdc6c6d39cdfbd4bfeb248a5b5854098083562ac";
              sha256 = "sha256-ZH5VnYTOtAxqetKlnqL2FJHeguX7G789Mj7b+riNEpM=";
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
    autoUpgrade = {
      enable = true;
      flags = [ "--update-input" "nixpkgs" "--commit-lock-file" ];
    };
    userActivationScripts = {
      reloadWallpaper.text = "${pkgs.xwallpaper}/bin/xwallpaper --zoom ${theme.wallpaper}";
      reloadXMonad = {
        text = "${pkgs.xmonad-with-packages}/bin/xmonad --restart";
        deps = [ "reloadWallpaper" ];
      };
    };
    stateVersion = "21.05";
  };
  systemd = {
    extraConfig = "RebootWatchdogSec=5";
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
      "realtime"
    ];
  };
  zramSwap = {
    enable = true;
    memoryPercent = 100;
  };
}
