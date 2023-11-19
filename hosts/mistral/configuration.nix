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

        "bukkit.yml" = pkgs.writeText "bukkit.yml" ''
          settings:
            allow-end: true
            warn-on-overload: true
            permissions-file: permissions.yml
            update-folder: update
            plugin-profiling: false
            connection-throttle: 4000
            query-plugins: true
            deprecated-verbose: default
            shutdown-message: bye bye...
            minimum-api: none
            use-map-color-cache: true
          spawn-limits:
            monsters: 35
            animals: 10
            water-animals: 5
            water-ambient: 20
            water-underground-creature: 5
            axolotls: 5
            ambient: 15
          chunk-gc:
            period-in-ticks: 600
          ticks-per:
            animal-spawns: 500
            monster-spawns: 1
            water-spawns: 1
            water-ambient-spawns: 1
            water-underground-creature-spawns: 1
            axolotl-spawns: 1
            ambient-spawns: 1
            autosave: 6000
          aliases: now-in-commands.yml
        '';

        "spigot.yml" = pkgs.writeText "spigot.yml" ''
          settings:
            debug: false
            timeout-time: 60
            restart-on-crash: true
            restart-script: ./start.sh
            save-user-cache-on-stop-only: false
            sample-count: 12
            bungeecord: false
            player-shuffle: 0
            user-cache-size: 1000
            moved-wrongly-threshold: 0.0625
            moved-too-quickly-multiplier: 10.0
            netty-threads: 4
            attribute:
              maxHealth:
                max: 2048.0
              movementSpeed:
                max: 2048.0
              attackDamage:
                max: 2048.0
            log-villager-deaths: true
            log-named-deaths: true
          messages:
            whitelist: You are not whitelisted on this server!
            unknown-command: Unknown command. Type "/help" for help.
            server-full: The server is full!
            outdated-client: Outdated client! Please use {0}
            outdated-server: Outdated server! I'm still on {0}
            restart: Server is restarting
          commands:
            replace-commands:
            - setblock
            - summon
            - testforblock
            - tellraw
            spam-exclusions:
            - /skill
            silent-commandblock-console: false
            log: true
            tab-complete: 0
            send-namespaced: true
          advancements:
            disable-saving: false
            disabled:
            - minecraft:story/disabled
          players:
            disable-saving: false
          world-settings:
            default:
              below-zero-generation-in-existing-chunks: true
              merge-radius:
                item: 2.5
                exp: 3.0
              view-distance: default
              simulation-distance: default
              mob-spawn-range: 5
              item-despawn-rate: 6000
              arrow-despawn-rate: 1200
              trident-despawn-rate: 1200
              zombie-aggressive-towards-villager: true
              nerf-spawner-mobs: true
              enable-zombie-pigmen-portal-spawns: true
              wither-spawn-sound-radius: 0
              end-portal-sound-radius: 0
              hanging-tick-frequency: 100
              thunder-chance: 100000
              growth:
                cactus-modifier: 100
                cane-modifier: 100
                melon-modifier: 100
                mushroom-modifier: 100
                pumpkin-modifier: 100
                sapling-modifier: 100
                beetroot-modifier: 100
                carrot-modifier: 100
                potato-modifier: 100
                torchflower-modifier: 100
                wheat-modifier: 100
                netherwart-modifier: 100
                vine-modifier: 100
                cocoa-modifier: 100
                bamboo-modifier: 100
                sweetberry-modifier: 100
                kelp-modifier: 100
                twistingvines-modifier: 100
                weepingvines-modifier: 100
                cavevines-modifier: 100
                glowberry-modifier: 100
                pitcherplant-modifier: 100
              entity-activation-range:
                animals: 32
                monsters: 32
                raiders: 48
                misc: 16
                water: 16
                villagers: 32
                flying-monsters: 32
                wake-up-inactive:
                  animals-max-per-tick: 4
                  animals-every: 1200
                  animals-for: 100
                  monsters-max-per-tick: 8
                  monsters-every: 400
                  monsters-for: 100
                  villagers-max-per-tick: 4
                  villagers-every: 600
                  villagers-for: 100
                  flying-monsters-max-per-tick: 8
                  flying-monsters-every: 200
                  flying-monsters-for: 100
                villagers-work-immunity-after: 100
                villagers-work-immunity-for: 20
                villagers-active-for-panic: true
                tick-inactive-villagers: true
                ignore-spectators: false
              entity-tracking-range:
                players: 48
                animals: 48
                monsters: 48
                misc: 32
                display: 128
                other: 64
              ticks-per:
                hopper-transfer: 8
                hopper-check: 1
              hopper-amount: 1
              hopper-can-load-chunks: false
              dragon-death-sound-radius: 0
              seed-village: 10387312
              seed-desert: 14357617
              seed-igloo: 14357618
              seed-jungle: 14357619
              seed-swamp: 14357620
              seed-monument: 10387313
              seed-shipwreck: 165745295
              seed-ocean: 14357621
              seed-outpost: 165745296
              seed-endcity: 10387313
              seed-slime: 987234911
              seed-nether: 30084232
              seed-mansion: 10387319
              seed-fossil: 14357921
              seed-portal: 34222645
              seed-ancientcity: 20083232
              seed-trailruins: 83469867
              seed-buriedtreasure: 10387320
              seed-mineshaft: default
              seed-stronghold: default
              hunger:
                jump-walk-exhaustion: 0.05
                jump-sprint-exhaustion: 0.2
                combat-exhaustion: 0.1
                regen-exhaustion: 6.0
                swim-multiplier: 0.01
                sprint-multiplier: 0.1
                other-multiplier: 0.0
              max-tnt-per-tick: 100
              max-tick-time:
                tile: 50
                entity: 50
              verbose: false
          config-version: 12
          stats:
            disable-saving: false
            forced-stats: {}
        '';

        "config/paper-global.yml" = pkgs.writeText "paper-global.yml" ''
          _version: 28
          block-updates:
            disable-chorus-plant-updates: false
            disable-mushroom-block-updates: false
            disable-noteblock-updates: false
            disable-tripwire-updates: false
          chunk-loading-advanced:
            auto-config-send-distance: true
            player-max-concurrent-chunk-generates: 10
            player-max-concurrent-chunk-loads: 15
          chunk-loading-basic:
            player-max-chunk-generate-rate: 15.0
            player-max-chunk-load-rate: 100.0
            player-max-chunk-send-rate: 75.0
          chunk-system:
            gen-parallelism: default
            io-threads: -1
            worker-threads: -1
          collisions:
            enable-player-collisions: true
            send-full-pos-for-hard-colliding-entities: true
          commands:
            fix-target-selector-tag-completion: true
            suggest-player-names-when-null-tab-completions: true
            time-command-affects-all-worlds: false
          console:
            enable-brigadier-completions: true
            enable-brigadier-highlighting: true
            has-all-permissions: false
          item-validation:
            book:
              author: 8192
              page: 16384
              title: 8192
            book-size:
              page-max: 2560
              total-multiplier: 0.98
            display-name: 8192
            lore-line: 8192
            resolve-selectors-in-books: false
          logging:
            deobfuscate-stacktraces: true
            log-player-ip-addresses: true
          messages:
            kick:
              authentication-servers-down: <lang:multiplayer.disconnect.authservers_down>
              connection-throttle: Connection throttled! Please wait before reconnecting.
              flying-player: <lang:multiplayer.disconnect.flying>
              flying-vehicle: <lang:multiplayer.disconnect.flying>
            no-permission: <red>hoy bawal lagi.
            use-display-name-in-quit-message: false
          misc:
            chat-threads:
              chat-executor-core-size: -1
              chat-executor-max-size: -1
            compression-level: default
            fix-entity-position-desync: true
            load-permissions-yml-before-plugins: true
            max-joins-per-tick: 5
            region-file-cache-size: 256
            strict-advancement-dimension-check: false
            use-alternative-luck-formula: false
            use-dimension-type-for-custom-spawners: false
          packet-limiter:
            all-packets:
              action: KICK
              interval: 7.0
              max-packet-rate: 500.0
            kick-message: <red><lang:disconnect.exceeded_packet_rate>
            overrides:
              ServerboundPlaceRecipePacket:
                action: DROP
                interval: 4.0
                max-packet-rate: 5.0
          player-auto-save:
            max-per-tick: -1
            rate: -1
          proxies:
            bungee-cord:
              online-mode: true
            proxy-protocol: false
            velocity:
              enabled: false
              online-mode: false
              secret: '''
          scoreboards:
            save-empty-scoreboard-teams: false
            track-plugin-scoreboards: false
          spam-limiter:
            incoming-packet-threshold: 300
            recipe-spam-increment: 1
            recipe-spam-limit: 20
            tab-spam-increment: 1
            tab-spam-limit: 500
          timings:
            enabled: false
            hidden-config-entries:
            - database
            - proxies.velocity.secret
            history-interval: 300
            history-length: 3600
            server-name: Unknown Server
            server-name-privacy: false
            url: https://timings.aikar.co/
            verbose: true
          unsupported-settings:
            allow-grindstone-overstacking: false
            allow-headless-pistons: false
            allow-permanent-block-break-exploits: false
            allow-piston-duplication: false
            compression-format: ZLIB
            perform-username-validation: true
          watchdog:
            early-warning-delay: 10000
            early-warning-every: 5000
        '';
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

