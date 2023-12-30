{ inputs, modulesPath, lib, pkgs, ... }:

{
  imports = lib.optional (__pathExists ./do-userdata.nix) ./do-userdata.nix ++ [
    (modulesPath + "/virtualisation/digital-ocean-config.nix")
  ];

  boot = {
    # NOTE: replace this with your desired kernel, see: https://nixos.wiki/wiki/Linux_kernel for reference.
    kernelPackages = pkgs.linuxKernel.packages.linux_lqx;

    kernelParams = [
      "preempt=full"
      "mitigations=off"
      "quiet"
      "udev.log_level=3"
    ];
  };

  environment.systemPackages = [ pkgs.tmux ];

  nixpkgs.overlays = lib.mkOverride 10 [ inputs.nix-minecraft.overlay ];

  programs.fish.enable = true;

  networking.firewall = {
    allowedTCPPorts = [ 4747 ];
    allowedUDPPorts = [ 4747 ];
  };

  services = {
    gonic = {
      enable = true;

      settings = {
        cache-path = "/var/cache/gonic";
        listen-addr = "128.199.204.196:4747";
        music-path = [ "/mnt/music" ];
        podcast-path = "/mnt/podcasts";
      };
    };

    minecraft-servers = {
      enable = true;
      eula = true;

      servers.volta = {
        enable = true;
        autoStart = false;
        package = pkgs.paperServers.paper-1_20_1;
        openFirewall = true;

        jvmOpts = lib.concatStringsSep " " [
          "-Xms2500M"
          "-Xmx2500M"
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
          view-distance = 8;
          simulation-distance = 8;
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

          "plugins/spark.jar" = pkgs.fetchurl {
            url = "https://ci.lucko.me/job/spark/396/artifact/spark-bukkit/build/libs/spark-1.10.55-bukkit.jar";
            hash = "sha256-M49tu1FZPRJErpZAJB3QmtuKi98yqfXYZlEftcinGfY=";
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
            url = "https://ci.ender.zone/job/EssentialsX/lastSuccessfulBuild/artifact/jars/EssentialsX-2.21.0-dev+24-0af4436.jar";
            hash = "sha256-3Nw/oK9vC3zKcqcPaL/a38uJ6dlZRPDcsf5AYH/ArvE=";
          };

          "plugins/essentialsx-chat.jar" = pkgs.fetchurl {
            url = "https://ci.ender.zone/job/EssentialsX/lastSuccessfulBuild/artifact/jars/EssentialsXChat-2.21.0-dev+21-c68b277.jar";
            hash = "sha256-U4A/ll4OCdnSPOstZsVJhXWlxB4sPJn35oUvkq3UY5U=";
          };

          "plugins/essentialx-protect.jar" = pkgs.fetchurl {
            url = "https://ci.ender.zone/job/EssentialsX/lastSuccessfulBuild/artifact/jars/EssentialsXProtect-2.21.0-dev+21-c68b277.jar";
            hash = "sha256-DPNpMvO4rkWoGhsn02Z09h37Fwmdq2/PDmSWLMvxBNk=";
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
            url = "https://download.luckperms.net/1521/bukkit/loader/LuckPerms-Bukkit-5.4.108.jar";
            hash = "sha256-TN7HH/5JiG98xBACfuoJZILsiDxU8WX5laNDS3h+qR4=";
          };

          "plugins/placeholderapi.jar" = pkgs.fetchurl {
            url = "https://hangarcdn.papermc.io/plugins/HelpChat/PlaceholderAPI/versions/2.11.5/PAPER/PlaceholderAPI-2.11.5.jar";
            hash = "sha256-RDVvvForTFvaqdkwLB1G2o2fpD1JiudaGZPtHy5XhO8=";
          };

          "plugins/multiworld.jar" = pkgs.fetchurl {
            url = "https://hangarcdn.papermc.io/plugins/Multiverse/Multiverse-Core/versions/4.3.12/PAPER/multiverse-core-4.3.12.jar";
            hash = "sha256-mCN6rzXG7nv9lft/OZ73A7PnK/+Oq0iKkEqtnUUwzRA=";
          };

          "bukkit.yml" = ./config/bukkit.yml;
          "spigot.yml" = ./config/spigot.yml;
          "config/paper-global.yml" = ./config/paper-global.yml;
          "plugins/Essentials/config.yml" = ./config/essentials-config.yml;
          "plugins/WorldEdit/config.yml" = ./config/worldedit-config.yml;
          "plugins/DeathChest/config.yml" = ./config/deathchest-config.yml;
        };
      };
    };
  };

  users.users.moni = {
    isNormalUser = true;
    home = "/home/moni";
    shell = pkgs.fish;
    extraGroups = [ "wheel" ];
  };
}

