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
        "-Xms3G"
        "-Xmx3G"
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
        view-distance = 10;
        spawn-protection = 5;
      };

      files = {
        "plugins/spark.jar" = pkgs.fetchurl {
          url = "https://ci.lucko.me/job/spark/396/artifact/spark-bukkit/build/libs/spark-1.10.55-bukkit.jar";
          sha256 = "sha256-M49tu1FZPRJErpZAJB3QmtuKi98yqfXYZlEftcinGfY=";
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
