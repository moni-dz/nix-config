{ inputs, modulesPath, lib, pkgs, ... }:

{
  imports = lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix ++ [
    (modulesPath + "/virtualisation/digital-ocean-config.nix")
  ];

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
