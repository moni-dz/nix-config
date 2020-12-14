# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, options, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./cachix.nix
    ];

  # Allow nonfree software
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      (self: super: {
        picom = super.picom.overrideAttrs (old: {
          src = super.fetchFromGitHub {
            owner = "jonaburg";
            repo = "picom";
            rev = "a8445684fe18946604848efb73ace9457b29bf80";
            sha256 = "154s67p3lxdv9is3lnc32j48p7v9n18ga1j8ln1dxcnb38c19rj7";
          };
        });
      })
    ];
  };
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''experimental-features = nix-command flakes'';
    trustedUsers = [ "root" "fortuneteller2k" ];
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    autoOptimiseStore = true;
  };

  # Use the grub EFI boot loader.
  boot = {
    kernelPackages = pkgs.linuxPackages_zen;
    kernelParams = [
      "mitigations=off"
      "acpi_backlight=vendor"
    ];
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
 
  # Set your time zone.
  time.timeZone = "Asia/Manila";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    hostName = "nixos"; # Set your hostname
    nameservers = [ "1.1.1.1" "1.0.0.1" ];
    networkmanager = {
      enable = true; # Enable wireless with NetworkManager
      dns = "none";
    };
    useDHCP = false;
    interfaces = {
      eno1.useDHCP = true;
      wlo1.useDHCP = true;
    };
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services = {
    xserver = {
      # Enable selected window managers.
      enable = true;
      displayManager = {
        gdm.enable = true;
        defaultSession = "none+xmonad";
      };
      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
        };
      };
      # Configure keymap in X11
      layout = "us";
      # Enable touchpad support (enabled default in most desktopManager).
      libinput.enable = true;
    };
    picom = {
      enable = true;
      experimentalBackends = true;
      backend = "glx";
      vSync = true;
      settings = {
        transition-length = 150;
        spawn-center-screen = true;
        blur = {
          method = "dual_kawase";
          strength = 5;
          background = false;
          background-frame = false;
          background-fixed = false;
        };
        blur-background-exclude = [ "window_type = 'dock'" "window_type = 'desktop'" ];
      };
    };
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
    openssh.enable = true;
  };

  # Enable sound.
  sound.enable = true;
  hardware = { 
    pulseaudio.enable = true; # Use pulseaudio for sound
    acpilight.enable = true; # Backlight control
    opengl = {
      enable = true;
      extraPackages = with pkgs; [
        vaapiVdpau
        libvdpau-va-gl
      ];
    };
  };

  # sudo needs no password
  security.sudo.wheelNeedsPassword = false;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.fortuneteller2k = {
    isNormalUser = true;
    home = "/home/fortuneteller2k";
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "video" "audio" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    coreutils
    git
    xdo 
    vim
    lxappearance
    shotgun
    hacksaw
    xclip
    pcmanfm
    cmake
    unzip
    ripgrep
    fd
    python3
    nixfmt
    shellcheck
    gcc
    rustup
    rust-analyzer-unwrapped
    elixir
    nodejs
    nodePackages.typescript
    nodePackages.npm
    go
    gimp
    gnumake
    libtool
    xorg.xkill
    xidlehook
    xss-lock
    nitrogen
  ];

  programs = {
    slock.enable = true; # screen locker
    zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
      autosuggestions.enable = true;
      promptInit = "eval $(starship init zsh)";
      interactiveShellInit = ''
        export PATH=$PATH:$HOME/.config/emacs/bin
      '';
      shellAliases = {
        ls = "exa";
        la = "exa -la";
        l = "exa -l";
      };
    };
  };

  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  
  # Font packages and configuration
  fonts = {
    fonts = with pkgs; [
      nerdfonts
      inter
      fantasque-sans-mono
      twemoji-color-font
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        serif = [ "Inter" ];
        sansSerif = [ "Inter" ];
        monospace = [ "FantasqueSansMono Nerd Font" ];
        emoji = [ "Twitter Color Emoji" ];
      };
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system = {
    stateVersion = "20.09"; # Did you read the comment?
    autoUpgrade.enable = true;
  };
}
