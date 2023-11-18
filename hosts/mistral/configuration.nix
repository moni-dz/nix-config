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
        "-Xmx1800M"
        "-Xms1G"
      ];

      serverProperties = {
        motd = "moni";
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
    };
  };

  users.users.moni = {
    isNormalUser = true;
    home = "/home/moni";
    shell = pkgs.fish;
    extraGroups = [ "wheel" ];
  };
}
