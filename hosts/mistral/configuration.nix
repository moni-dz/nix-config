{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

{
  imports = [ ./hardware-configuration.nix ];

  boot = {
    # NOTE: replace this with your desired kernel, see: https://nixos.wiki/wiki/Linux_kernel for reference.
    kernelPackages = pkgs.linuxKernel.packages.linux_hardened;

    kernelParams = [
      "preempt=full"
      "quiet"
      "udev.log_level=3"
    ];

    loader.grub.device = "/dev/sda";
  };

  environment.systemPackages = [ pkgs.tmux ];

  nixpkgs.overlays = lib.mkOverride 10 [
    inputs.nix-minecraft.overlay
    inputs.crowdsec.overlays.default
  ];

  programs = {
    fish.enable = true;
    mosh.enable = true;
  };

  networking.firewall.allowedTCPPorts = [
    2022
    # 1433
    # 4747
    # 5432
  ];

  systemd.services.crowdsec.serviceConfig = {
    ExecStartPre =
      let
        script = pkgs.writeScriptBin "register-bouncer" ''
          #!${pkgs.runtimeShell}
          set -eu
          set -o pipefail

          if ! cscli bouncers list | grep -q "tough-guy"; then
            cscli bouncers add "tough-guy" --key "$(cat ${config.age.secrets.bouncer.path})"
          fi
        '';
      in
      [ "${script}/bin/register-bouncer" ];
  };

  services = {
    dbus.implementation = "broker";

    crowdsec = {
      enable = true;
      enrollKeyFile = config.age.secrets.crowdsec.path;
    };

    crowdsec-firewall-bouncer = {
      enable = false;
      settings = {
        api_url = "http://localhost:8080";
        api_key = __readFile config.age.secrets.bouncer.path; # yes, this is an antipattern, but if someone's in you are fucked anyway...
      };
    };

    eternal-terminal.enable = true;

    openssh = {
      enable = true;
      settings = {
        GatewayPorts = "yes";
        PasswordAuthentication = false;
        PermitRootLogin = "yes";
      };
    };

    postgresql = {
      enable = false;
      package = pkgs.postgresql_17;
      enableTCPIP = true;
    };

    minecraft-servers = {
      enable = false;
      eula = true;

      servers.volta = {
        enable = true;
        autoStart = false;
        package = pkgs.paperServers.paper-1_20_1;
        openFirewall = true;

        jvmOpts = lib.concatStringsSep " " [
          "-Xms4800M"
          "-Xmx4800M"
          "-XX:+UseG1GC"
          "-XX:+ParallelRefProcEnabled"
          "-XX:MaxGCPauseMillis=200"
          "-XX:+UnlockExperimentalVMOptions"
          "-XX:+DisableExplicitGC"
          "-XX:+AlwaysPreTouch"
          "-XX:G1NewSizePercent=30"
          "-XX:G1MaxNewSizePercent=40"
          "-XX:G1HeapRegionSize=8M"
          "-XX:G1ReservePercent=20"
          "-XX:G1HeapWastePercent=5"
          "-XX:G1MixedGCCountTarget=4"
          "-XX:InitiatingHeapOccupancyPercent=15"
          "-XX:G1MixedGCLiveThresholdPercent=90"
          "-XX:G1RSetUpdatingPauseTimePercent=5"
          "-XX:SurvivorRatio=32"
          "-XX:+PerfDisableSharedMem"
          "-XX:MaxTenuringThreshold=1"
          "-Dusing.aikars.flags=https://mcflags.emc.gs"
          "-Daikars.new.flags=true"
        ];

        serverProperties = {
          motd = "juice";
          server-port = 43000;
          online-mode = false;
          max-players = 20;
          difficulty = "normal";
          gamemode = "survival";
          enable-rcon = true;
          "rcon.password" = "longview";
          view-distance = 16;
          simulation-distance = 16;
          spawn-protection = 5;
          allow-flight = true;
        };

        files = {
          "plugins/worldedit.jar" = pkgs.fetchurl {
            url = "https://dev.bukkit.org/projects/worldedit/files/4793142/download";
            hash = "sha256-8X0+URLekKrp8Ab1io3ejW7zoAQnv26wsjTHhN0B+Ho=";
          };

          "plugins/worldprotect.jar" = pkgs.fetchurl {
            url = "https://dev.bukkit.org/projects/worldguard/files/4675318/download";
            hash = "sha256-DATTDAyYwh6CDexvfjgJk0FqKT1+JkaEn23NTv3CQoc=";
          };

          "plugins/coreprotect.jar" = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/Lu3KuzdV/versions/w3P6ufP1/CoreProtect-22.2.jar";
            hash = "sha512-Bw0xC+ooj1bNs0Qr+Apz8WsaDR2CrId4VZJLRwxIPVHzGZPDSHEGPIBBqh/mjXCmJ7nB/p3vlDgZtew/zwPxnQ==";
          };

          "plugins/viaversion.jar" = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/P1OZGk5p/versions/5ELLKlnY/ViaVersion-4.9.3-SNAPSHOT.jar";
            hash = "sha512-e4QzpI+rtOhVde8gFiOz3nAoofB89R6/EkkXFPmddCJlnrWL0CTSyhtdMha5xQsRtxxLIJ4Kd662PxGZ/QPL+w==";
          };

          "plugins/vault.jar" = pkgs.fetchurl {
            url = "https://github.com/MilkBowl/Vault/releases/download/1.7.3/Vault.jar";
            hash = "sha256-prXtl/Q6XPW7rwCnyM0jxa/JvQA/hJh1r4s25s930B0=";
          };

          "plugins/essentialsx.jar" = pkgs.fetchurl {
            url = "https://ci.ender.zone/job/EssentialsX/lastSuccessfulBuild/artifact/jars/EssentialsX-2.21.0-dev+102-fcf6e64.jar";
            hash = "sha256-XHPS96loP1pzGQ507ioVuJ3xGikG5w1dtMHPhMdFlbo";
          };

          "plugins/essentialsx-chat.jar" = pkgs.fetchurl {
            url = "https://ci.ender.zone/job/EssentialsX/lastSuccessfulBuild/artifact/jars/EssentialsXChat-2.21.0-dev+102-fcf6e64.jar";
            hash = "sha256-iQY4iBK3Ehp06MUcBKfh9pHq+QuwI47rDbDVNKUtLiA";
          };

          "plugins/essentialx-protect.jar" = pkgs.fetchurl {
            url = "https://ci.ender.zone/job/EssentialsX/lastSuccessfulBuild/artifact/jars/EssentialsXProtect-2.21.0-dev+102-fcf6e64.jar";
            hash = "sha256-wDqFnVlsaT1LHSVfnQxrd8IU2y5ZBqZ8OltAJjydrkA";
          };

          "plugins/deathchest.jar" = pkgs.fetchurl {
            url = "https://hangarcdn.papermc.io/plugins/CyntrixAlgorithm/DeathChest/versions/2.1.1/PAPER/deathchest.jar";
            hash = "sha256-djHwoB3AsLBEtegyaWaIBkAwM2WTL8vqtCtJrHduNSw=";
          };

          "plugins/chunky.jar" = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/fALzjamp/versions/B0xkCkk4/Chunky-1.3.92.jar";
            hash = "sha512-Y8QYSQICdqbwFW/F0Rq+93ylPBq0SiQsl8pSJmaAnClCeAXP84st5ITH1eLz5+s+uKmJq8/rvSBBlxiIHhKwCw==";
          };

          "plugins/luckperms.jar" = pkgs.fetchurl {
            url = "https://download.luckperms.net/1551/bukkit/loader/LuckPerms-Bukkit-5.4.136.jar";
            hash = "sha256-Zr7wGzriaVeeFZE5Wm424w3A2ow4GocWDjkpN9hxknQ";
          };

          "plugins/placeholderapi.jar" = pkgs.fetchurl {
            url = "https://hangarcdn.papermc.io/plugins/HelpChat/PlaceholderAPI/versions/2.11.5/PAPER/PlaceholderAPI-2.11.5.jar";
            hash = "sha256-RDVvvForTFvaqdkwLB1G2o2fpD1JiudaGZPtHy5XhO8=";
          };

          "plugins/multiworld.jar" = pkgs.fetchurl {
            url = "https://hangarcdn.papermc.io/plugins/Multiverse/Multiverse-Core/versions/4.3.12/PAPER/multiverse-core-4.3.12.jar";
            hash = "sha256-mCN6rzXG7nv9lft/OZ73A7PnK/+Oq0iKkEqtnUUwzRA=";
          };

          "plugins/multiworld-inventories.jar" = pkgs.fetchurl {
            url = "https://hangarcdn.papermc.io/plugins/Multiverse/Multiverse-Inventories/versions/4.2.6/PAPER/multiverse-inventories-4.2.6.jar";
            hash = "sha256-Lh73k4g9iRFpDMtfi5OJMG9blauRXwZ4df8zt87Ep+g=";
          };

          "plugins/multiworld-signportals.jar" = pkgs.fetchurl {
            url = "https://hangarcdn.papermc.io/plugins/Multiverse/Multiverse-SignPortals/versions/4.2.2/PAPER/multiverse-signportals-4.2.2.jar";
            hash = "sha256-FJkS5kzCfET7eObqEnVW5K5ziNnp5B3baxPjQgMRL6s=";
          };

          "plugins/mcmmo.jar" = pkgs.requireFile {
            url = "https://mcmmo.org";
            name = "mcMMO-2.1.225.jar";
            hash = "sha256-PoVKTcIxQEzn8OCNM1herUG7+ojD1JmVRY3xrQ2I1uQ=";
          };

          "bukkit.yml" = ./config/bukkit.yml;
          "spigot.yml" = ./config/spigot.yml;
          "config/paper-global.yml" = ./config/paper-global.yml;
          "plugins/Essentials/config.yml" = ./config/essentials-config.yml;
          "plugins/WorldEdit/config.yml" = ./config/worldedit-config.yml;
          "plugins/DeathChest/config.yml" = ./config/deathchest-config.yml;
          "plugins/Multiverse-Core/config.yml" = ./config/multiverse-core.yml;
        };
      };
    };
  };

  users.users.moni = {
    isNormalUser = true;
    home = "/home/moni";
    shell = pkgs.fish;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBlr5SHXali3ttWt8ygyWgCW2usWVsBhXebeyi2XKO2Z lythe1107@gmail.com"
    ];
  };
}
