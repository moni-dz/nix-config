{ inputs, ... }:

{
  flake.overlays = {
    default = _final: prev: {
      # the dkms output builds the kernel module from the same source
      bcachefs-tools = prev.bcachefs-tools.overrideAttrs (old: {
        version = "1.39.7-unstable-${inputs.bcachefs-tools.shortRev}";
        src = inputs.bcachefs-tools;
        cargoDeps = prev.rustPlatform.fetchCargoVendor {
          src = inputs.bcachefs-tools;
          hash = "sha256-zqDByJ9jPbzPggtnZP54FzZYp1GoV9q+YvLMXU2IE6s=";
        };
      });

      lib = prev.lib.extend (
        _lfinal: _lprev: {
          inherit ((import "${inputs.infuse.outPath}/default.nix" { inherit (inputs.nixpkgs) lib; }).v1)
            infuse
            ;
        }
      );
    };
  };
}
