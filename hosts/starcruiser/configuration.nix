{
  config,
  lib,
  pkgs,
  options,
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
  imports = [
    ./hardware-configuration.nix

    # Append your custom NixOS modules in this list
    # ../../modules/nixos/programs/river.nix
  ];

  boot = {
    kernelModules = [ "amd-pstate" ];

    kernelParams = [
      "amd_pstate=passive"
      "amd_pstate.shared_mem=1"
      "initcall_blacklist=acpi_cpufreq_init"
    ];
  };

  hardware = {
    cpu.amd.updateMicrocode = true;

    /*
      hardware-configuration.nix enables this by default because of this line:

      >  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

      See https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/installer/scan/not-detected.nix
    */
    enableRedistributableFirmware = true;

    nvidia = {
      modesetting.enable = true;
      open = true;
      package = config.boot.kernelPackages.nvidiaPackages.beta;
    };

    graphics.enable = true;
  };

  # Font packages should go in fonts.packages in ../shared/configuration.nix.
  environment = {
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
      WLR_NO_HARDWARE_CURSORS = "1";
      NVD_BACKEND = "direct";
      LIBVA_DRIVER_NAME = "nvidia";
      MOZ_DISABLE_RDD_SANDBOX = "1";
      MOZ_GLX_TEST_EARLY_WL_ROUNDTRIP = "1";
    };

    systemPackages = __attrValues {
      inherit (pkgs)
        file
        nautilus
        ntfs3g
        pavucontrol
        pulseaudio
        ripgrep
        util-linux
        unrar
        unzip
        xarchiver
        zip
        ;

      inherit (pkgs.qt5) qtwayland;
    };
  };

  fonts.enableDefaultPackages = false;

  networking = {
    firewall = {
      allowedTCPPorts = [
        445
        139
      ];
      allowedUDPPorts = [
        137
        138
      ];
    };

    nameservers = [
      "1.1.1.1"
      "1.0.0.1"
      "8.8.8.8"
      "8.8.4.4"
    ];
  };

  nix.sshServe = {
    enable = true;
    protocol = "ssh-ng";
    keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBlr5SHXali3ttWt8ygyWgCW2usWVsBhXebeyi2XKO2Z lythe1107@gmail.com"
    ];
  };

  # https://github.com/nix-community/home-manager/issues/1288#issuecomment-636352427
  programs.sway.enable = true;

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      function export
        if [ $argv ]
          set var (echo $argv | cut -f1 -d=)
          set val (echo $argv | cut -f2 -d=)
          set -g -x $var $val
        else
          echo 'export var=value'
        end
      end

      . ${config.age.secrets.tokens.path}
    '';
  };

  services = {
    avahi = {
      enable = true;
      nssmdns4 = true;

      publish = {
        enable = true;
        addresses = true;
        domain = true;
        hinfo = true;
        userServices = true;
        workstation = true;
      };

      extraServiceFiles = {
        smb = ''
          <?xml version="1.0" standalone='no'?><!--*-nxml-*-->
          <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
          <service-group>
            <name replace-wildcards="yes">%h</name>
            <service>
              <type>_smb._tcp</type>
              <port>445</port>
            </service>
          </service-group>
        '';
      };
    };

    greetd = {
      enable = true;

      settings =
        let
          sway-cmd = lib.concatStringsSep " " [
            (lib.getExe config.programs.sway.package)
            "--unsupported-gpu"
          ];
        in
        {
          default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd '${sway-cmd}'";

          initial_session = {
            command = "${sway-cmd}";
            user = "moni";
          };
        };
    };

    samba = {
      enable = true;

      # You will still need to set up the user accounts to begin with:
      # $ sudo smbpasswd -a yourusername

      # This adds to the [global] section:
      settings."global" = {
        browseable = "yes";
        "smb encrypt" = "required";
      };

      shares = {
        homes = {
          browseable = "no";
          "read only" = "no";
          "guest ok" = "no";
        };
      };
    };

    samba-wsdd.enable = true;

    xserver.videoDrivers = [ "nvidia" ];
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
}
