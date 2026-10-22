{ inputs, ... }:

{
  flake.overlays = {
    default = _final: prev: {
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
