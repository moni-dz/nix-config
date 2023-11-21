{ inputs, modulesPath, lib, pkgs, ... }:

{
  imports = lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix ++ [
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

  nixpkgs.overlays = lib.mkOverride 10 [ inputs.nix-minecraft.overlay ];

  programs.fish.enable = true;

  services.minecraft-servers = {
    enable = true;
    eula = true;

    servers.volta = {
      enable = true;
      autoStart = true;
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
        motd = "hihihi - moni";
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
      };

      files = {
        "plugins/worldedit.jar" = pkgs.fetchurl {
          url = "https://dev.bukkit.org/projects/worldedit/files/4793142/download";
          sha256 = "sha256-8X0+URLekKrp8Ab1io3ejW7zoAQnv26wsjTHhN0B+Ho=";
        };

        "plugins/worldprotect.jar" = pkgs.fetchurl {
          url = "https://dev.bukkit.org/projects/worldguard/files/4675318/download";
          sha256 = "sha256-DATTDAyYwh6CDexvfjgJk0FqKT1+JkaEn23NTv3CQoc=";
        };

        "plugins/coreprotect.jar" = pkgs.fetchurl {
          url = "https://cdn.modrinth.com/data/Lu3KuzdV/versions/w3P6ufP1/CoreProtect-22.2.jar";
          sha512 = "070d310bea288f56cdb3442bf80a73f16b1a0d1d82ac877855924b470c483d51f31993c34871063c8041aa1fe68d70a627b9c1fe9def943819b5ec3fcf03f19d";
        };

        "plugins/spark.jar" = pkgs.fetchurl {
          url = "https://ci.lucko.me/job/spark/396/artifact/spark-bukkit/build/libs/spark-1.10.55-bukkit.jar";
          sha256 = "sha256-M49tu1FZPRJErpZAJB3QmtuKi98yqfXYZlEftcinGfY=";
        };

        "plugins/viaversion.jar" = pkgs.fetchurl {
          url = "https://cdn.modrinth.com/data/P1OZGk5p/versions/DXFf7cQP/ViaVersion-4.9.0-23w45a-SNAPSHOT.jar";
          sha512 = "abf015d197e7cc037b7a6f12f4354853d0b1d276547f87cc32b3e874ba241de1ebf50f35eca3cd7b41ae26cd4acfaf8f6bba9ba1797394ec4e8260c21bf1e3ff";
        };

        "plugins/essentialsx.jar" = pkgs.fetchurl {
          url = "https://ci.ender.zone/job/EssentialsX/lastSuccessfulBuild/artifact/jars/EssentialsX-2.21.0-dev+21-c68b277.jar";
          sha256 = "sha256-7gvyn6Ys2+kIJiXJ0kZoLoXEHo8+BDXfSMbAOdpite8=";
        };

        "plugins/chunky.jar" = pkgs.fetchurl {
          url = "https://cdn.modrinth.com/data/fALzjamp/versions/B0xkCkk4/Chunky-1.3.92.jar";
          sha512 = "63c41849020276a6f0156fc5d11abef77ca53c1ab44a242c97ca522666809c29427805cff38b2de484c7d5e2f3e7eb3eb8a989abcfebbd20419718881e12b00b";
        };

        "plugins/luckperms.jar" = pkgs.fetchurl {
          url = "https://download.luckperms.net/1521/bukkit/loader/LuckPerms-Bukkit-5.4.108.jar";
          sha256 = "sha256-TN7HH/5JiG98xBACfuoJZILsiDxU8WX5laNDS3h+qR4=";
        };

        "bukkit.yml" = ./config/bukkit.yml;
        "spigot.yml" = ./config/spigot.yml;
        "config/paper-global.yml" = ./config/paper-global.yml;
        "plugins/Essentials/config.yml" = ./config/essentials-config.yml;
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

