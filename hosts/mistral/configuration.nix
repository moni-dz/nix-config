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
        view-distance = 10;
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

        "config/paper-global.yml" = pkgs.writeText "paper-global.yml" ''
          _version: 28
          block-updates:
            disable-chorus-plant-updates: false
            disable-mushroom-block-updates: false
            disable-noteblock-updates: false
            disable-tripwire-updates: false
          chunk-loading-advanced:
            auto-config-send-distance: true
            player-max-concurrent-chunk-generates: 2
            player-max-concurrent-chunk-loads: 2
          chunk-loading-basic:
            player-max-chunk-generate-rate: 6.0
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

