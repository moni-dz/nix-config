{ self, inputs, ... }:

{
  imports = [ ./overlays.nix ];

  perSystem =
    { pkgs, ... }:
    {
      packages = removeAttrs (self.overlays.default pkgs pkgs) [ "lib" ];
    };
}
