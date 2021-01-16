{ config, pkgs, options, ... }:

{
  imports = [ ./hardware-configuration.nix ];
  nix.package = pkgs.nixFlakes;
  boot = {
    kernelPackages = pkgs.linuxPackages_zen;
    kernelParams = [
      "rw"
      "mitigations=off"
      "acpi_backlight=vendor"
      "vt.default_red=0x16,0xe9,0x29,0xfa,0x26,0xee,0x59,0xfd,0x23,0xec,0x3f,0xfb,0x3f,0xf0,0x6b,0xfd"
      "vt.default_grn=0x16,0x56,0xd3,0xb7,0xbb,0x64,0xe3,0xf0,0x35,0x6a,0xda,0xc3,0xc6,0x75,0xe6,0xf0"
      "vt.default_blu=0x1c,0x78,0x98,0x95,0xd9,0xae,0xe3,0xed,0x30,0x88,0xa4,0xa7,0xde,0xb7,0xe6,0xed"
    ];
    loader = {
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        device = "nodev";
        efiSupport = true;
        useOSProber = true;
        gfxmodeEfi = "1366x768";
        extraConfig = ''GRUB_CMDLINE_LINUX="reboot=bios"'';
      };
    };
  };
  time.timeZone = "Asia/Manila";
  networking = {
    hostName = "superfluous";
    nameservers = [ "1.1.1.1" "1.0.0.1" ];
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
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  services = {
    dbus.packages = with pkgs; [ gnome3.dconf ];
    xserver = {
      enable = true;
      dpi = 96;
      displayManager = {
        session = [ { 
          manage = "window";
          name = "river";
          start = "${pkgs.river}/bin/river";
        } ];
        lightdm = {
          enable = true;
          background = ./config/wallpapers/horizon.jpg;
          greeters.gtk = {
            enable = true;
            iconTheme = {
              name = "Papirus";
              package = pkgs.papirus-icon-theme;
            };
            theme = {
              name = "fortuneteller2k_phocus";
              package = pkgs.phocus;
            };
          };
        };
        defaultSession = "none+xmonad";
      };
      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
          config = (import ./config/xmonad.nix);
          extraPackages = hpkgs: with hpkgs; [ dbus monad-logger ];
        };
      };
      layout = "us";
      libinput.enable = true;
    };
    picom = (import ./config/picom-vsync.nix);
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
    tlp.enable = true;
    openssh.enable = true;
  };
  sound.enable = true;
  hardware = {
    pulseaudio.enable = true;
    opengl = {
      enable = true;
      extraPackages = with pkgs; [ vaapiVdpau libvdpau-va-gl ];
    };
  };
  security = {
    sudo.wheelNeedsPassword = false;
    doas = {
      enable = true;
      wheelNeedsPassword = false;
    };
  };
  users.users.fortuneteller2k = {
    isNormalUser = true;
    home = "/home/fortuneteller2k";
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "video" "audio" ];
  };
  environment.systemPackages = with pkgs; [
    envsubst
    ffmpeg
    wget
    curl
    coreutils
    util-linux
    brightnessctl
    pciutils
    git
    xdo
    xdotool
    imagemagick
    notify-desktop
    vim
    shotgun
    hacksaw
    xclip
    pcmanfm
    python39Packages.grip
    pandoc
    jq
    cmake
    unzip
    zip
    ripgrep
    fd
    nixfmt
    python3
    shellcheck
    gcc
    ccls
    rustup
    rust-analyzer-unwrapped
    elixir
    ntfs3g
    nodejs
    nodePackages.typescript
    nodePackages.npm
    go
    gnumake
    libtool
    xorg.xkill
    xorg.xdpyinfo
    xorg.xwininfo
    xidlehook
    xwallpaper
    xmonad-log
    ghc
    stack
    haskell-language-server
    font-manager
    zig
  ];
  programs = {
    slock.enable = true;
    xss-lock = {
      enable = true;
      lockerCommand = "${pkgs.slock}/bin/slock";
    };
    zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
      autosuggestions.enable = true;
      promptInit = "eval $(starship init zsh)";
      interactiveShellInit = ''
        eval $(dircolors /etc/nixos/nixos/config/LS_COLORS)
        export PATH=$PATH:$HOME/.config/emacs/bin
      '';
      shellAliases = {
        ls = "exa --icons";
        la = "exa --icons -la";
        l = "exa --icons -l";
      };
    };
    sway = {
      enable = true;
      wrapperFeatures.gtk = true;
      extraPackages = with pkgs; [
        swaylock
        swayidle
        swaybg
        wl-clipboard
        mako
        grim
        slurp
        sway-contrib.grimshot
        waybar
        wofi
      ];
    };
  };
  powerManagement.powertop.enable = true;
  fonts = {
    fonts = with pkgs; [
      nerdfonts
      inter
      fantasque-sans-mono
      xorg.fontbh100dpi
      mplus-outline-fonts
      twemoji-color-font
    ];
    fontconfig = {
      enable = true;
      dpi = 96;
      defaultFonts = {
        serif = [ "Inter" ];
        sansSerif = [ "Inter" ];
        monospace = [ "FantasqueSansMono Nerd Font" ];
        emoji = [ "Twitter Color Emoji" ];
      };
    };
  };
  system = {
    stateVersion = "20.09";
    autoUpgrade = {
      enable = true;
      flags = [ "--update-input" "nixpkgs" "--commit-lock-file" ];
    };
  };
}
