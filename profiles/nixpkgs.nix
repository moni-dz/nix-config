# nixpkgs configuration (not the flake input)
{ lib, self, ... }:

{
  nixpkgs = {
    config = lib.mkForce {
      allowBroken = true;
      allowUnfree = true;

      # Experimental options, disable if you don't know what you are doing!
      contentAddressedByDefault = false;
    };

    overlays = [ self.overlays.default ];
  };
}
