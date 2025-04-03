{ lib, pkgs, ... }:

{
  services.minecraft-servers = {
    enable = true;
    eula = true;

    servers =
      let
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
          view-distance = 32;
          simulation-distance = 24;
          spawn-protection = 5;
          allow-flight = true;
          keep-inventory = true;
        };
      in
      {
        bettermc = let
          modpack = pkgs.fetchPackwizModpack {
            url = "https://raw.githubusercontent.com/moni-dz/bmc3-packwiz/refs/heads/master/pack.toml";
            packHash = "";
          };
        in {
          enable = true;
          autoStart = true;
          package = pkgs.fabricServers.fabric-1_21_1;
          openFirewall = true;

          inherit jvmOpts serverProperties;

          symlinks = {
            "mods" = "${modpack}/mods";
          };
        };
        cobble = {
          enable = false;
          autoStart = false;
          package = pkgs.fabricServers.fabric-1_21_1;
          openFirewall = true;

          inherit jvmOpts serverProperties;

          symlinks = {
            "mods/architectury.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/lhGA9TYQ/versions/Wto0RchG/architectury-13.0.8-fabric.jar";
              sha512 = "7a24a0481732c5504b07347d64a2843c10c29e748018af8e5f5844e5ea2f4517433886231025d823f90eb0b0271d1fa9849c27e7b0c81476c73753f79f19302a";
            };

            "mods/appleskin.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/EsAfCjCV/versions/b5ZiCjAr/appleskin-fabric-mc1.21-3.0.6.jar";
              sha512 = "accbb36b863bdeaaeb001f7552534f3bdf0f27556795cf8e813f9b32e7732450ec5133da5e0ec9b92dc22588c48ffb61577c375f596dc351f15c15ce6a6f4228";
            };

            "mods/ccme.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/VSNURh3q/versions/HqusQu7H/c2me-fabric-mc1.21.1-0.3.0%2Balpha.0.317.jar";
              sha512 = "c4cd79e20a8b69ecedf507cad8d4d532db5dab2c1e34c5a254e6f8414cafd9f9cd01c0394e730c16340211d397bd30960e247c09bf66c48e455e481e3a9bf6db";
            };

            "mods/fabric-api.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/P7dR8mSH/versions/aHOmYIWr/fabric-api-0.115.1%2B1.21.1.jar";
              sha512 = "54e039c309e3001089cd97b382cd0193c4fa5eaa92eeb351409588d8a203cdd81e7133d0479bd66331d487a69259e4b7b2db138afec99707f4d43dafc11aa13b";
            };

            "mods/fabric-kotlin.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/Ha28R6CL/versions/476dzMG5/fabric-language-kotlin-1.13.1%2Bkotlin.2.1.10.jar";
              sha512 = "8e3609ef53a731c5509b304397e7fd4e37f2bbb4353b0d6234e74438846f0464743022f3339ba4f5acf21b023c80420ce59c194c1dfb11aeb79caffa6f842fb6";
            };

            "mods/ferritecore.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/uXXizFIs/versions/bwKMSBhn/ferritecore-7.0.2-hotfix-fabric.jar";
              sha512 = "ca975bd3708cd96d30cf1447ac8883572113562eb2dd697e60c1cf382d6b70d0b1a511fcbfd042c51b2cf5d5ffc718b847f845e4c8a3e421e8c9ee741119a421";
            };

            "mods/cobblemon.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/MdwFAVRL/versions/v77SHSXW/Cobblemon-fabric-1.6.1%2B1.21.1.jar";
              sha512 = "b7082befee07efd3e0c5857807f739082df5994ec0e7fe3217ce6cdaec7d2ca47ed51bd129096fd4eca8cbcf7de415d14602868a3357980ff88e55b3148dc7f4";
            };

            "mods/cobblemon-xp.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/LBl4Qguc/versions/XJKujgmB/cobblemon-capturexp-1.6-fabric-1.0.0.jar";
              sha512 = "9a5c9a6264fb959ca27d5eb0c065a1dd4f002fb2c3e3564c0c00fc6e1a08b9e38d023a3ed273cf3c857d29688ba99d07383004abda8cc5b954a44845fe7092c3";
            };

            "mods/cobblemon-counter.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/rj8uLYP4/versions/6kSMeH0Q/cobblemon-counter-1.6-fabric-1.3.1.jar";
              sha512 = "cd94a999dfed4af11ff4b7a51dea70dfa144a891b482e13f6819d0d6aca73286a53f0cfc480655e4cbba127a144fe2b79bfbfe6f5b928c3d325d99f6fa83e214";
            };

            "mods/cobblemon-unchained.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/wh0wnzrT/versions/yqyJSngh/cobblemon-unchained-1.6-fabric-1.0.1.jar";
              sha512 = "a70b1d731a00c1760f2b87c879a298fc176ad0ae09de0686d74dc69cbbef357ee088a908c6bd31c14ed680b4320e5adde485ed4c17999e0552be2e0266f9f251";
            };

            "mods/cobblemon-fight-or-flight.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/cTdIg5HZ/versions/wvVbdpeV/fightorflight-fabric-0.7.4.jar";
              sha512 = "2b6aa9af37febdf3fb47a830276f9f0395f3ca98ee2a7903db6ef03d361a164817e62dc6f97fb4b6100a4a3efb842dd140f934415c2bc6ea573a569af1354e6a";
            };

            "mods/cobblemon-rider.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/ZLu8WiYO/versions/kjtNSQr4/cobblemonridingfabric-1.3.4.jar";
              sha512 = "9b57431176fe22fdd173bafa2244c6261f5bb63a1e9d3bdb2d824c931b49ebb8cddd746ca53ee0d9564327c9a7b1d3a898b4f84cc91e19bf8c870ea859cebbcd";
            };

            "mods/cobblemon-spawn-notification.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/LPuJjiQz/versions/7Vq83ds8/cobblemon-spawn-notification-1.6-fabric-1.0.0.jar";
              sha512 = "a14a7f00fb17f670a5704f833cb5dde007615fa48beddb8c0687654d5258748b43bdd7f555050ed63ad2d9f2f8efcfad2936de4c0303f008b863e73867a3009d";
            };

            "mods/owo-lib.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/ccKDOlHs/versions/vCCHsvEa/owo-lib-0.12.15%2B1.21.jar";
              sha512 = "faa86df062ec4fa58b653455deb1307a778e4307392f51a29c5a87de78da7173bb6f77a5ed239306226f2bf972aa9221fbd946a2c07232ecfa09f798ae381924";
            };

            "mods/lavender.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/D5h9NKNI/versions/gdB0WW0x/lavender-0.1.15%2B1.21.jar";
              sha512 = "5fe79ac7b8c150de305b317884d32b16c5d89c3b284f0d7ea4f2ded07e44750ac6a6e1b91993706700da0bf69329552a09e7476f6ed0d7911f7d425cb187c060";
            };

            "mods/knowlogy.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/V4OTaHaq/versions/v1gchzmq/knowlogy-fabric-0.7.1-1.21.1.jar";
              sha512 = "b05d0cc49ea80eceebc7e639b4548b8701d866bcf97a4d1d390c5e644505356a643b35e57b447701ea8b52e3af3533012543b1e04e9c50bb95a2e51d08aed520";
            };

            "mods/krypton.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/fQEb0iXm/versions/Acz3ttTp/krypton-0.2.8.jar";
              sha512 = "5f8cf96c79bfd4d893f1d70da582e62026bed36af49a7fa7b1e00fb6efb28d9ad6a1eec147020496b4fe38693d33fe6bfcd1eebbd93475612ee44290c2483784";
            };

            "mods/lithium.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/gvQqBUqZ/versions/3HMQZXbw/lithium-fabric-0.14.8%2Bmc1.21.1.jar";
              sha512 = "ed33ab7cc57950ec8b04b41f751720a3f903158093f1940b1a0bdb938b94bfaf398fc238014fee4523c9407fd3ff2377ee2e09cf7f7a349bfc8fc9e5a0b55a19";
            };

            "mods/scalablelux.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/Ps1zyz6x/versions/Oh80nTJ5/ScalableLux-0.1.0%2Bfabric.26c6e72-all.jar";
              sha512 = "586b45d542827f65b08d9ecae2c2367133b7a204da064b6d578dcf0119629e0b595343035e84dc4a86254c1e21cd64c5ed393fd746b28d5472f1e119b3d08923";
            };

            "mods/threadtweak.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/vSEH1ERy/versions/F4sjmsi3/threadtweak-fabric-0.1.5%2Bmc1.21.1.jar";
              sha512 = "b0221075239b9998d08e9a42d7bb3205c22482dc39f4b62a1c57c1f7444c9ec9cdee4a245b6b9c6b23f61f3cec82056c40cfc09e6c1bc0690cd936dfed6393a1";
            };

            "mods/modernfix.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/nmDcB62a/versions/tn4FFqcg/modernfix-fabric-5.20.2%2Bmc1.21.1.jar";
              sha512 = "1ed6f19b08198d07b6a45a3fe6aaee01f420e70bc73c8cafd1be46ccea97584f407005395eb787b0c4a90e5d9ed9581938d9b335369468d1b05e935d7792e15a";
            };

            "mods/xaeros-minimap.jar" = pkgs.fetchurl {
              url = "https://cdn.modrinth.com/data/1bokaNcj/versions/2Go558kv/Xaeros_Minimap_25.1.0_Fabric_1.21.jar";
              sha512 = "9c1f2b8dc8f3f28a8f73293981249c07cb70b48952f3e2a9973c2ab38d9aa0d38e72fd538247cfd39b1d6f712d9bc76968ea1bbd81b598a85de2f4750af4b513";
            };
          };
        };

        volta = {
          enable = false;
          autoStart = false;
          package = pkgs.paperServers.paper-1_20_1;
          openFirewall = true;

          inherit jvmOpts serverProperties;

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
}
