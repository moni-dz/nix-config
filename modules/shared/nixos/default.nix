{
  config,
  lib,
  pkgs,
  options,
  inputs,
  inputs',
  system,
  ...
}:

/*
  NixOS configuration

  Useful links:
  - Package Search: https://search.nixos.org/packages?channel=unstable
  - Options Search: https://search.nixos.org/options?channel=unstable
*/
{
  boot = {
    consoleLogLevel = 0;

    initrd = {
      systemd.enable = true;
      verbose = false;
    };

    /*
      NOTE: replace this with your desired kernel, see: https://nixos.wiki/wiki/Linux_kernel for reference.

      If you're not me or a XanMod kernel maintainer in Nixpkgs, use pkgs.linuxKernel.packages.linux_xanmod instead to avoid compilation.
    */
    kernelPackages = pkgs.linuxKernel.packages.linux_xanmod_latest;

    kernelParams = [
      "preempt=full"
      "mitigations=off"
      "quiet"
      "udev.log_level=3"
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

      systemd-boot = {
        enable = true;
        configurationLimit = 3;
        consoleMode = "max";
        editor = false;
      };
    };
  };

  console = {
    keyMap = "us";

    colors =
      let
        colorscheme = inputs.nix-colors.colorSchemes.material-darker;
      in
      with colorscheme.palette;
      [
        base01
        base08
        base0B
        base0A
        base0D
        base0E
        base0C
        base06
        base02
        base08
        base0B
        base0A
        base0D
        base0E
        base0C
        base07
      ];
  };

  documentation.man =
    let
      activeManOutputs = [ "man" ] ++ lib.optionals config.documentation.dev.enable [ "devman" ];
    in
    {
      generateCaches = true;

      man-db.manualPages = pkgs.buildEnv {
        name = "man-paths";
        paths = config.environment.systemPackages;
        pathsToLink = [ "/share/man" ];
        extraOutputsToInstall = activeManOutputs;
        ignoreCollisions = true;
      };
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
    shells = with pkgs; [ zsh ];
  };

  fonts = {
    packages = __attrValues {
      inherit (pkgs)
        emacs-all-the-icons-fonts
        fantasque-sans-mono
        # NOTE: use only when current is outdated
        # iosevka-ft
        # iosevka-ft-qp
        sarasa-gothic
        symbola
        terminus_font
        twemoji-color-font
        ;

      inherit (inputs'.nixpkgs-f2k.packages)
        iosevka-ft-bin
        iosevka-ft-qp-bin
        ;

      nerdfonts = pkgs.nerdfonts.override {
        fonts = [
          "FantasqueSansMono"
          "Iosevka"
        ];
      };
    };

    fontconfig = {
      enable = lib.mkDefault true;

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

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

  powerManagement.cpuFreqGovernor = "performance";

  programs = {
    bash.interactiveShellInit = ''export HISTFILE=$HOME/.config/.bash_history'';
    command-not-found.enable = false;
    dconf.enable = true;
    ssh.pubkeyAcceptedKeyTypes = [ "ssh-ed25519" ];
    fish.enable = true;
  };

  qt.platformTheme = "qt5ct";

  security = {
    sudo.wheelNeedsPassword = false;
    polkit.enable = true;
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

    dbus.implementation = "broker";

    journald.extraConfig = lib.mkForce "";

    openssh = {
      enable = true;
      settings = {
        GatewayPorts = "yes";
        PermitRootLogin = "yes";
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
    };

    usbmuxd.enable = true;
  };

  systemd = {
    services.rtkit-daemon.serviceConfig.ExecStart = [
      ""
      "${pkgs.rtkit}/libexec/rtkit-daemon --our-realtime-priority=95 --max-realtime-priority=90"
    ];

    user.services = {
      pipewire.wantedBy = [ "default.target" ];
      pipewire-pulse.wantedBy = [ "default.target" ];
    };
  };

  time = {
    hardwareClockInLocalTime = true;
    timeZone = "Asia/Manila";
  };

  users.users.moni = {
    isNormalUser = true;
    home = "/home/moni";
    shell = pkgs.fish;

    extraGroups = [
      "wheel"
      "video"
      "audio"
      "realtime"
    ];
  };

  zramSwap = {
    enable = true;
    memoryPercent = 40;
  };
}
