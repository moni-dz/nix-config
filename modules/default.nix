# Reusable modules, exported as flake outputs. See the README for what each one needs.
{
  flake = {
    hjemModules.default = ./hjem;

    nixosModules = {
      identity = ./common/identity.nix;
      hjem = ./nixos/hjem.nix;
      rex-libpam-wrapper = ./nixos/rex-libpam-wrapper.nix;
    };

    darwinModules = {
      identity = ./common/identity.nix;
      hjem = ./darwin/hjem.nix;
    };
  };
}
