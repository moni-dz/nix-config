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
    kernelPackages = pkgs.linuxKernel.packages.linux_xanmod_latest;

    kernelParams = [
      "preempt=full"
      "mitigations=off"
      "quiet"
      "udev.log_level=3"
    ];

    kernel.sysctl = {
      "fs.file-max" = 2097152;
      "kernel.printk" = "3 3 3 3";
      "kernel.sched_migration_cost_ns" = 5000000;
      "kernel.sched_nr_fork_threshold" = 3;
      "kernel.sched_fake_interactive_win_time_ms" = 1000;
      "kernel.unprivileged_userns_clone" = 1;
      "net.core.default_qdisc" = "fq_pie";
      "vm.dirty_ratio" = 60;
      "vm.dirty_background_ratio" = 2;
      "vm.swappiness" = 10;
      "vm.vfs_cache_pressure" = 75;
      "net.core.netdev_max_backlog" = 16384;
      "net.core.somaxconn" = 8192;
      "net.core.rmem_default" = 1048576;
      "net.core.rmem_max" = 16777216;
      "net.core.wmem_default" = 1048576;
      "net.core.wmem_max" = 16777216;
      "net.core.optmem_max" = 65536;
      "net.ipv4.tcp_rmem" = "4096 1048576 2097152";
      "net.ipv4.tcp_wmem" = "4096 65536 16777216";
      "net.ipv4.udp_rmem_min" = 8192;
      "net.ipv4.udp_wmem_min" = 8192;
      "net.ipv4.tcp_fastopen" = 3;
      "net.ipv4.tcp_keepalive_time" = 60;
      "net.ipv4.tcp_keepalive_intvl" = 10;
      "net.ipv4.tcp_keepalive_probes" = 6;
      "net.ipv4.conf.default.log_martians" = 1;
      "net.ipv4.conf.all.log_martians" = 1;
      "net.ipv4.tcp_mtu_probing" = 1;
      "net.ipv4.tcp_syncookies" = 1;
      "net.ipv4.tcp_congestion_control" = "bbr2";
    };
  };

  nixpkgs.overlays = lib.mkOverride 10 [ inputs.nix-minecraft.overlay ];

  services.minecraft-servers = {
    enable = true;
    eula = true;

    servers.volta = {
      enable = true;
      autoStart = true;
      package = pkgs.fabricServers.fabric-1_20_2;
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
        view-distance = 20;
      };

      symlinks = {
        mods = pkgs.linkFarmFromDrvs "mods" (__attrValues {
          Starlight = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/H8CaAYZC/versions/PLbxwptm/starlight-1.1.3%2Bfabric.5867eae.jar";
            sha512 = "bb9426b5218550d8f9baa3022604feec9f72ac1f1efea07ee70d9871040628d1db039b8c78f30593ab7a5dd4706317a141a4681b6c3adab3bfe7d862003e89e7";
          };

          Lithium = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/gvQqBUqZ/versions/qdzL5Hkg/lithium-fabric-mc1.20.2-0.12.0.jar";
            sha512 = "88df5f96ee5a3011dbb2aae011b5c85166f9942906e4ebc58ebb7b452f01e18020b970aad3facebd02eb67ac4beea03de333414cf66172d817fa5cae50e1c73d";
          };

          FerriteCore = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/uXXizFIs/versions/unerR5MN/ferritecore-6.0.1-fabric.jar";
            sha512 = "9b7dc686bfa7937815d88c7bbc6908857cd6646b05e7a96ddbdcada328a385bd4ba056532cd1d7df9d2d7f4265fd48bd49ff683f217f6d4e817177b87f6bc457";
          };

          Krypton = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/fQEb0iXm/versions/cQ60Ouax/krypton-0.2.4.jar";
            sha512 = "37a076ea08f7f49aebc8b0a1519ae7d1844bf169134b152f446dc7b95d37567808b96e8523001b98ebd19950420eb76da35df47e8d9b9af0846e68c7c829d7c0";
          };

          LazyDFU = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/hvFnDODi/versions/0.1.3/lazydfu-0.1.3.jar";
            sha512 = "dc3766352c645f6da92b13000dffa80584ee58093c925c2154eb3c125a2b2f9a3af298202e2658b039c6ee41e81ca9a2e9d4b942561f7085239dd4421e0cce0a";
            };

          C2ME = pkgs.fetchurl {
            url = "https://cdn.modrinth.com/data/VSNURh3q/versions/ilKwGRiJ/c2me-fabric-mc1.20.2-0.2.0%2Balpha.10.126.jar";
            sha512 = "c29eb0f1bd77f083118a3a7664ed92bb23c2bfa82a4fb3dde44f505f486c92301c2e51b24d1d5bd0fbb80c22da93bb787583aa8e6ff5f8913c7315e23184bebe";
          };
        });
      };
    };
  };
}
