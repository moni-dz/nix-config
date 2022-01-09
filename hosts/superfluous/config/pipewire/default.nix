{
  pipewire."context.properties" = {
    "core.daemon" = true;
    "core.name" = "pipewire-0";
    "link.max-buffers" = 16;
    "log.level" = 2;
    "default.clock.rate" = 48000;
    "default.clock.quantum" = 32;
    "default.clock.min-quantum" = 32;
    "default.clock.max-quantum" = 32;
  };

  pipewire-pulse = {
    "context.modules" = [
      { name = "libpipewire-module-protocol-native"; }
      { name = "libpipewire-module-client-node"; }
      { name = "libpipewire-module-adapter"; }
      { name = "libpipewire-module-metadata"; }
      { name = "libpipewire-module-profiler"; }
      { name = "libpipewire-module-spa-device-factory"; }
      { name = "libpipewire-module-spa-node-factory"; }
      { name = "libpipewire-module-client-device"; }
      { name = "libpipewire-module-link-factory"; }
      { name = "libpipewire-module-session-manager"; }

      {
        name = "libpipewire-module-portal";
        flags = [ "ifexists" "nofail" ];
      }

      {
        name = "libpipewire-module-access";
        args = { };
      }

      {
        name = "libpipewire-module-rtkit";

        args = {
          "nice.level" = -15;
          "rt.prio" = 90;
          "rt.time.soft" = 200000;
          "rt.time.hard" = 200000;
        };

        flags = [ "ifexists" "nofail" ];
      }

      {
        name = "libpipewire-module-protocol-pulse";
        args = {
          "server.address" = [ "unix:native" ];
          "pulse.min.req" = "32/48000";
          "pulse.min.quantum" = "32/48000";
          "pulse.min.frag" = "32/48000";
          "pulse.default.req" = "2048/48000";
          "pulse.default.frag" = "2048/48000";
        };
      }
    ];

    "stream.properties" = {
      "node.latency" = "32/48000";
      "resample.quality" = 1;
    };
  };
}
