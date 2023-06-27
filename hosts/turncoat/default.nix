{ inputs, withSystem, ... }:

{
  flake.nixosConfigurations.turncoat = withSystem "x86_64-linux" ({ inputs', system, nixpkgs-config, overlays, ... }@args:
    # See https://github.com/NixOS/nixpkgs/blob/master/flake.nix#L24 for reference.
    let
      inherit (inputs) agenix nixpkgs nixos-wsl;
    in
    nixpkgs.lib.nixosSystem {
      inherit system;

      modules = [
        agenix.nixosModules.age
        nixos-wsl.nixosModules.wsl

        ({ lib, ... }: {
          # Extra arguments passed to the module system
          _module.args = {
            inherit inputs inputs' system;
            inherit (args) master unstable stable;
          };

          # NOTE: you should either change this or disable it completely by commenting it out
          age = {
            identityPaths = [ "/home/zero/.ssh/id_ed25519" ];

            secrets.github-token = {
              file = ../../secrets/github-token.age;
              owner = "zero";
              mode = "0444";
            };
          };

          nix = import ../../nix-settings.nix {
            inherit inputs inputs' system nixpkgs;
          };

          nixpkgs = {
            inherit overlays;
            config = nixpkgs-config;
          };

          networking.hostName = "turncoat";
          system.stateVersion = "22.05";

          wsl = {
            enable = true;
            defaultUser = "zero";
            startMenuLaunchers = true;

            wslConf = {
              network.hostname = "turncoat";
              automount.root = "/mnt";
            };
          };
        })

        ./configuration.nix
      ];
    }
  );
}
