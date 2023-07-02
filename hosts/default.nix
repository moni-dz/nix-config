{ inputs, ... }:

{
  # nix-darwin configurations
  parts.darwinConfigurations.shaker = {
    system = "aarch64-darwin";
    stateVersion = 4; # only change this if you know what you are doing.

    modules = [
      inputs.agenix.darwinModules.default

      {
        # NOTE: you should either change this or disable it completely by commenting it out
        age = {
          identityPaths = [ "/Users/moni/.ssh/id_ed25519" ];

          secrets.github-token = {
            file = ../secrets/github-token.age;
            owner = "moni";
            group = "staff";
            mode = "600";
          };
        };
      }

      ./shaker/configuration.nix
    ];
  };

  # NixOS configurations
  parts.nixosConfigurations = {
    starcruiser = {
      system = "x86_64-linux";
      stateVersion = "22.11"; # only change this if you know what you are doing.

      modules = [
        inputs.agenix.nixosModules.default

        {
          # NOTE: you should either change this or disable it completely by commenting it out
          age.secrets.github-token = {
            file = ../secrets/github-token.age;
            owner = "moni";
            mode = "0444";
          };
        }

        ./starcruiser/configuration.nix
      ];
    };

    turncoat = {
      system = "x86_64-linux";
      stateVersion = "22.05"; # only change this if you know what you are doing.

      modules = [
        inputs.agenix.nixosModules.default
        inputs.nixos-wsl.nixosModules.wsl

        {
          # NOTE: you should either change this or disable it completely by commenting it out
          age = {
            identityPaths = [ "/home/zero/.ssh/id_ed25519" ];

            secrets.github-token = {
              file = ../secrets/github-token.age;
              owner = "zero";
              mode = "0444";
            };
          };
        }

        ./turncoat/configuration.nix
      ];
    };
  };
}
