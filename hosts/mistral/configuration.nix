{ inputs, modulesPath, lib, pkgs, ... }:

{
  imports = lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix ++ [
    (modulesPath + "/virtualisation/digital-ocean-config.nix")
  ];

  boot = {
    /*
      NOTE: replace this with your desired kernel, see: https://nixos.wiki/wiki/Linux_kernel for reference.

      If you're not me or a XanMod kernel maintainer in Nixpkgs, use pkgs.linuxKernel.packages.linux_xanmod instead to avoid compilation.
    */
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
      package = pkgs.fabricServers.fabric-1_20_1;
      jvmOpts = "-Xmx1700M -Xms1G";
      openFirewall = true;

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

      symlinks = {
        mods = pkgs.linkFarmFromDrvs "mods" (__attrValues {
          Starlight = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/H8CaAYZC/versions/XGIsoVGT/starlight-1.1.2%2Bfabric.dbc156f.jar";
            sha512 = "6b0e363fc2d6cd2f73b466ab9ba4f16582bb079b8449b7f3ed6e11aa365734af66a9735a7203cf90f8bc9b24e7ce6409eb04d20f84e04c7c6b8e34f4cc8578bb";
          };

          Lithium = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/gvQqBUqZ/versions/ZSNsJrPI/lithium-fabric-mc1.20.1-0.11.2.jar";
            sha512 = "d1b5c90ba8b4879814df7fbf6e67412febbb2870e8131858c211130e9b5546e86b213b768b912fc7a2efa37831ad91caf28d6d71ba972274618ffd59937e5d0d";
          };

          FerriteCore = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/uXXizFIs/versions/unerR5MN/ferritecore-6.0.1-fabric.jar";
            sha512 = "9b7dc686bfa7937815d88c7bbc6908857cd6646b05e7a96ddbdcada328a385bd4ba056532cd1d7df9d2d7f4265fd48bd49ff683f217f6d4e817177b87f6bc457";
          };

          Krypton = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/fQEb0iXm/versions/jiDwS0W1/krypton-0.2.3.jar";
            sha512 = "92b73a70737cfc1daebca211bd1525de7684b554be392714ee29cbd558f2a27a8bdda22accbe9176d6e531d74f9bf77798c28c3e8559c970f607422b6038bc9e";
          };

          LazyDFU = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/hvFnDODi/versions/0.1.3/lazydfu-0.1.3.jar";
            sha512 = "dc3766352c645f6da92b13000dffa80584ee58093c925c2154eb3c125a2b2f9a3af298202e2658b039c6ee41e81ca9a2e9d4b942561f7085239dd4421e0cce0a";
          };

          C2ME = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/VSNURh3q/versions/T5Pkyhit/c2me-fabric-mc1.20.1-0.2.0%2Balpha.11.0.jar";
            sha512 = "9ea27bc6c794a3f428fc1c41d2d083b70e51883d1e836b02b7f556208de012fd28f2d9c9d42b71e33c7b52dd32efcd2e20932e56b16665ff935d0428ff583157";
          };

          Fastload = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/kCpssoSb/versions/ys9T20o4/Fastload%2B1.18.2-1.20-3.4.0.jar";
            sha512 = "024e9aa775037fe8727ac8aca55f68d2f6782dd87d099446f967ebbeb6eab73f0f0580eca100cb565f70bab02abce03239af0fca17248ceef2c05caa1cfcdd1e";
          };

          MemoryLeakFix = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/NRjRiSSD/versions/dGlflhb6/memoryleakfix-fabric-1.17%2B-1.1.2.jar";
            sha512 = "a921d171d4391a690724cde818d43f666b03a36e1ffa05849c2c853f6e005f6fc4254f1bc040890a0a8a6eff57abf2edf73367189bb2ec9e34e9df92920d89ad";
          };

          VeryManyPlayers = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/wnEe9KBa/versions/sV8lIBhJ/vmp-fabric-mc1.20.1-0.2.0%2Bbeta.7.102-all.jar";
            sha512 = "38ba14d870ddbcef233c9baa399005aaccde95d7b77175aa3a795eb9fed1086492b4ac5c6bbc05909dd76dce1b19b5c26613585161c2f3d20f2494beb8b23fe4";
          };
        });
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
