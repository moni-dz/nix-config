{
  pipewire."context.properties" = {
    "link.max-buffers" = 16;
    "default.clock.min-quantum" = "32/48000";
    "default.clock.max-quantum" = "2048/48000";
  };

  pipewire-pulse = {
    "context.modules" = [
      { name = "libpipewire-module-protocol-native"; }
      { name = "libpipewire-module-client-node"; }
      { name = "libpipewire-module-adapter"; }
      { name = "libpipewire-module-metadata"; }

      {
        name = "libpipewire-module-rtkit";
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

    "stream.properties"."node.latency" = "32/48000";
  };
}
