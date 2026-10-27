{
  self,
  inputs,
  withSystem,
  ...
}:

let
  installers = {
    hands = {
      user = "lyt";
      defaultDevice = "/dev/nvme0n1";
    };

    hyper-v = {
      user = "moni";
      defaultDevice = "/dev/sda";
    };
  };
in
{
  flake = {
    nixosConfigurations = inputs.nixpkgs.lib.mapAttrs' (host: args: {
      name = "installer-${host}";
      value = withSystem "x86_64-linux" (
        { inputs', self', ... }:
        inputs.nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit
              inputs
              self
              inputs'
              self'
              ;
          };
          modules = [
            inputs.rex.nixosModules.default
            ../profiles/installer
            {
              nixpkgs.hostPlatform = "x86_64-linux";

              installer = {
                inherit host;
                inherit (args) user defaultDevice;
                target = self.nixosConfigurations.${host};
              };
            }
          ];
        }
      );
    }) installers;

    checks.x86_64-linux.installer-script = import ../profiles/installer/test.nix {
      pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
    };
  };
}
