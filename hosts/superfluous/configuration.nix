{ config, lib, pkgs, options, ... }:

let theme = import ../../config/theme.nix { inherit pkgs; };
in
{
  boot = {
    blacklistedKernelModules = [ "amdgpu" ];

    /*
      NOTE: replace this with your desired kernel

      If you're not me or a XanMod kernel maintainer in Nixpkgs, use pkgs.linuxPackages_xanmod instead.
    */
    # kernelPackages = pkgs.kernel.linuxPackages_xanmod;
    kernelPackages = pkgs.master.linuxPackages_xanmod;

    kernelParams = [
      "mitigations=off"
      "acpi_backlight=vendor"
      "nmi_watchdog=0"
      "systemd.watchdog-device=/dev/watchdog"
    ];

    kernel.sysctl = {
      "fs.file-max" = 2097152;
      "kernel.printk" = "3 3 3 3";
      "kernel.sched_migration_cost_ns" = 5000000;
      "kernel.sched_nr_fork_threshold" = 3;
      "kernel.sched_fake_interactive_win_time_ms" = 1000;
      "kernel.unprivileged_userns_clone" = 1;
      "net.core.default_qdisc" = "fq_pie";
      "vm.dirty_ratio" = 60;
      "vm.dirty_background_ratio" = 2;
      "vm.swappiness" = 10;
      "vm.vfs_cache_pressure" = 75;
      "net.core.netdev_max_backlog" = 16384;
      "net.core.somaxconn" = 8192;
      "net.core.rmem_default" = 1048576;
      "net.core.rmem_max" = 16777216;
      "net.core.wmem_default" = 1048576;
      "net.core.wmem_max" = 16777216;
      "net.core.optmem_max" = 65536;
      "net.ipv4.tcp_rmem" = "4096 1048576 2097152";
      "net.ipv4.tcp_wmem" = "4096 65536 16777216";
      "net.ipv4.udp_rmem_min" = 8192;
      "net.ipv4.udp_wmem_min" = 8192;
      "net.ipv4.tcp_fastopen" = 3;
      "net.ipv4.tcp_keepalive_time" = 60;
      "net.ipv4.tcp_keepalive_intvl" = 10;
      "net.ipv4.tcp_keepalive_probes" = 6;
      "net.ipv4.conf.default.log_martians" = 1;
      "net.ipv4.conf.all.log_martians" = 1;
      "net.ipv4.tcp_mtu_probing" = 1;
      "net.ipv4.tcp_syncookies" = 1;
      "net.ipv4.tcp_congestion_control" = "bbr2";
    };

    loader = {
      efi.canTouchEfiVariables = true;

      grub = {
        enable = true;
        configurationLimit = 5;
        copyKernels = true;
        device = "nodev";
        efiSupport = true;
        useOSProber = true;
        gfxmodeEfi = "1366x768";
      };
    };
  };

  console =
    let
      normal = with theme.colors; [ c0 c1 c2 c3 c4 c5 c6 c7 ];
      bright = with theme.colors; [ c8 c9 c10 c11 c12 c13 c14 c15 ];
    in
    {
      colors = normal ++ bright;
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

  documentation.man.generateCaches = true;

  hardware = {
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

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

  environment = {
    /*
      NOTE: This isn't found in https://search.nixos.org/options.

      Here's the warning that came with it:

      "Please note that NixOS assumes all over the place that shell to be Bash,
      so override the default setting only if you know exactly what you're doing."
    */
    binsh = "${pkgs.zsh}/bin/zsh";

    pathsToLink = [ "/share/zsh" ];

    sessionVariables = with pkgs; {
      # LD_PRELOAD = "/etc/nixos/config/ld-preload-xcreatewindow.so";
      _JAVA_AWT_WM_NONREPARENTING = "1";
    };

    shells = with pkgs; [ zsh ];

    # Font packages should go in fonts.fonts a few lines below this.
    systemPackages = with pkgs; [
      alsaTools
      alsaUtils
      brightnessctl
      coreutils
      curl
      dash
      fd
      file
      git
      glxinfo
      gnome3.nautilus
      hikari
      libva-utils
      lm_sensors
      man-pages
      man-pages-posix
      ntfs3g
      pavucontrol
      pciutils
      psmisc
      pulseaudio
      ripgrep
      subversion
      util-linux
      unrar
      unzip
      wget
      xarchiver
      zip
    ];
  };

  fonts = {
    fonts = with pkgs; [
      curie
      edwin
      emacs-all-the-icons-fonts
      fantasque-sans-mono
      (nerdfonts.override { fonts = [ "FantasqueSansMono" "Iosevka" ]; })
      iosevka-ft-bin
      # TODO: use only when current is outdated
      # iosevka-ft
      sarasa-gothic
      scientifica
      symbola
      terminus_font
      twemoji-color-font
    ];

    fontconfig = {
      enable = true;

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
    dhcpcd.enable = false;
    hostName = "superfluous";

    # Replace with your interface names.
    interfaces = {
      eno1.useDHCP = true;
      wlan0.useDHCP = true;
    };

    nameservers = [ "1.1.1.1" "1.0.0.1" "8.8.8.8" "8.8.4.4" ];

    networkmanager = {
      enable = true;
      dns = "none";
      wifi.backend = "iwd";
    };

    useDHCP = false;

    wireless.iwd = {
      enable = true;
      settings.Settings.AutoConnect = true;
    };
  };

  powerManagement.cpuFreqGovernor = "schedutil";

  programs = {
    bash.interactiveShellInit = ''export HISTFILE=$HOME/.config/.bash_history'';
    command-not-found.enable = false;
    slock.enable = config.services.xserver.enable;

    sway = {
      enable = true;
      wrapperFeatures.gtk = true;

      extraPackages = with pkgs; [
        autotiling
        swaylock
        swayidle
        swaybg
        wayland-utils
        wl-clipboard
        wf-recorder
        brightnessctl
        grim
        slurp
        sway-contrib.grimshot
        waybar
        bemenu
        qt5.qtwayland
        xdg_utils
      ];

      extraSessionCommands = ''
        export XDG_SESSION_DESKTOP=sway
        export SDL_VIDEODRIVER=wayland
        export QT_QPA_PLATFORM=wayland-egl
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        export MOZ_ENABLE_WAYLAND=1
        export CLUTTER_BACKEND=wayland
        export ECORE_EVAS_ENGINE=wayland-egl
        export ELM_ENGINE=wayland_egl
        export NO_AT_BRIDGE=1
      '';
    };

    qt5ct.enable = true;

    xss-lock = {
      enable = !config.programs.sway.enable;
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

    greetd = {
      enable = config.programs.sway.enable;

      settings = {
        default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";

        initial_session = {
          command = "sway";
          user = "fortuneteller2k";
        };
      };
    };

    journald.extraConfig = lib.mkForce "";

    openssh = {
      enable = true;
      gatewayPorts = "yes";
      permitRootLogin = "yes";
    };

    picom = {
      enable = config.services.xserver.enable;
      refreshRate = 60;
      experimentalBackends = config.services.picom.backend == "glx";
      backend = "glx";
      vSync = true;

      settings = import ./config/picom-settings.nix;
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

      config = import ./config/pipewire;
      media-session.config = import ./config/pipewire/media-session.nix;
    };

    thelounge.enable = true;
    usbmuxd.enable = true;
    upower.enable = true;

    xserver = {
      enable = !config.programs.sway.enable;

      displayManager = {
        sddm.enable = config.services.xserver.enable;
        defaultSession = "none+xmonad";
      };

      extraConfig = import ./config/xorg.nix;
      useGlamor = true;

      windowManager = {
        /*
          NOTE: I primarily use Sway

          See overlays/2bwm.nix for applied patches.
        */
        "2bwm".enable = false;

        # FIXME: bling can't be detected
        # awesome = {
        #   enable = false;
        #   luaModules = with pkgs; [ bling ];
        # };

        xmonad = with pkgs; {
          enable = config.services.xserver.enable;
          config = import ./config/xmonad.nix { inherit config pkgs theme; };
          # Don't use enableContribAndExtras since xmonad-extras doesn't compile on the git versions.
          extraPackages = hpkgs: with hpkgs; [ dbus xmonad-contrib ];

          ghcArgs = [
            "-O2"
            "-fasm-shortcutting"
            "-fdicts-cheap"
            "-fdicts-strict"
            "-funfolding-use-threshold=16"
            "-fspecialise-aggressively"
            "-fblock-layout-weightless"
            "-feager-blackholing"
            "-fexpose-all-unfoldings"
            "-fregs-iterative"
            "-fspec-constr-keen"
            "-fstatic-argument-transformation"
            "-funbox-strict-fields"
            "-flate-dmd-anal"
            "-fignore-interface-pragmas"
            "-ffun-to-thunk"
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
        let
          xwallpaperFlag = if theme.colors.tiledWallpaper then "--tile" else "--zoom";
        in
        if config.services.xserver.enable && !config.programs.sway.enable then
          "[ $DISPLAY ] && ${pkgs.xwallpaper}/bin/xwallpaper ${xwallpaperFlag} ${theme.colors.wallpaper} || ${pkgs.coreutils}/bin/echo 'skipping...'"
        else
          "${pkgs.coreutils}/bin/echo 'skipping because on wayland...'";

      reloadXMonad.text =
        if config.services.xserver.enable && config.services.xserver.windowManager.xmonad.enable then
          "[ $DISPLAY ] && ${pkgs.xmonad-with-packages}/bin/xmonad --restart || echo 'not in xmonad, skipping...' || ${pkgs.coreutils}/bin/echo 'skipping...'"
        else
          "${pkgs.coreutils}/bin/echo 'skipping because on wayland...'";
    };

    /*
      NOTE: DO NOT CHANGE THIS IF YOU DON'T KNOW WHAT YOU'RE DOING.

      Only change this if you are ABSOLUTELY 100% SURE that you don't have stateful data.
    */
    stateVersion = "21.05";
  };

  systemd = {
    extraConfig = "RebootWatchdogSec=5";
    services.rtkit-daemon = import ./services/rtkit-daemon.nix { inherit pkgs; };

    user.services = {
      pipewire.wantedBy = [ "default.target" ];
      pipewire-pulse.wantedBy = [ "default.target" ];
      # xidlehook = import ./services/xidlehook.nix { inherit config lib pkgs; };
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

  xdg.portal.enable = true;

  zramSwap = {
    enable = true;
    memoryPercent = 100;
  };
}
