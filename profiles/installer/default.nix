{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:

let
  cfg = config.installer;
  inherit (cfg) target;

  diskoScript =
    (target.extendModules {
      modules = [ { disko.devices.disk.main.device = lib.mkForce "/dev/@DEVICE@"; } ];
    }).config.system.build.diskoScript;

  secretFile =
    path:
    let
      file = builtins.path {
        inherit path;
        name = baseNameOf path;
      };
    in
    assert lib.assertMsg (
      !(lib.hasInfix "-source/" file)
    ) "${path} must not point into the flake source";
    file;

  installHost = import ./script.nix { inherit pkgs; } {
    targetSystem = target.config.system.build.toplevel;
    inherit diskoScript;
    inherit (cfg) user defaultDevice;
    hostName = cfg.host;
    passwordAge = secretFile ../../secrets/default-password.age;
    hostKeyAge = secretFile (../../secrets/host-keys + "/${cfg.host}.age");
    masterPubkey = (lib.head target.config.age.rekey.masterIdentities).pubkey;
  };

  readmeText =
    lib.replaceStrings [ "@host@" "@user@" "@defaultDevice@" ] [ cfg.host cfg.user cfg.defaultDevice ]
      (builtins.readFile ./README.txt);

  hasWifi = cfg.host == "hands";

  installerWifi = pkgs.writeShellApplication {
    name = "installer-wifi";
    runtimeInputs = [ pkgs.networkmanager ];
    text = ''
      read -r -p "SSID: " ssid
      read -r -s -p "Passphrase: " pass
      echo
      nmcli device wifi connect "$ssid" password "$pass"
    '';
  };
in
{
  imports = [ (modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix") ];

  options.installer = {
    host = lib.mkOption {
      type = lib.types.str;
      description = "Name of the host this ISO installs.";
    };

    user = lib.mkOption {
      type = lib.types.str;
      description = "Login user that gets the default password and the pasted key.";
    };

    defaultDevice = lib.mkOption {
      type = lib.types.str;
      description = "Device suggested when install-host asks for the target.";
    };

    rexPrincipal = lib.mkOption {
      type = lib.types.str;
      default = "tailscale:moni-dz@github";
      description = "Rex login server principal admitted as root on the installer.";
    };

    target = lib.mkOption {
      type = lib.types.raw;
      description = "The nixosConfiguration being installed.";
    };
  };

  config = lib.mkMerge [
    {
      networking.hostName = "installer-${cfg.host}";
      system.stateVersion = config.system.nixos.release;

      boot = {
        kernelPackages = lib.mkForce target.config.boot.kernelPackages;
        supportedFilesystems.bcachefs = true;
      };

      isoImage = {
        storeContents = [
          target.config.system.build.toplevel
          diskoScript
        ];
        squashfsCompression = "zstd -Xcompression-level 19";
      };

      environment = {
        systemPackages = [ installHost ] ++ lib.optional hasWifi installerWifi;
        etc."installer-README".text = readmeText;

        interactiveShellInit = ''
          if [ -z "''${INSTALLER_README_SHOWN:-}" ]; then
            export INSTALLER_README_SHOWN=1
            cat /etc/installer-README
          fi
        ''
        + lib.optionalString hasWifi ''
          if [ -z "$(ip route show default)" ]; then installer-wifi; fi
        '';
      };

      users.motd = readmeText;

      services.getty.autologinUser = lib.mkForce "root";

      services.tailscale.enable = true;

      systemd.services.installer-tailscale-up = {
        wantedBy = [ "multi-user.target" ];
        wants = [ "network-online.target" ];
        after = [
          "network-online.target"
          "tailscaled.service"
        ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          TimeoutStartSec = "infinity";
          StandardOutput = "journal+console";
          StandardError = "journal+console";
          ExecStart = "${lib.getExe config.services.tailscale.package} up --hostname=${config.networking.hostName}";
        };
      };

      services.rex.loginServer = {
        enable = true;
        accounts.root.principals = [ cfg.rexPrincipal ];
        tailscaleServe.enable = true;
      };
    }

    (lib.mkIf hasWifi {
      hardware.enableRedistributableFirmware = true;
      networking = {
        wireless.enable = lib.mkForce false;
        networkmanager.enable = true;
      };
    })
  ];
}
