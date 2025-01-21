{ self, inputs, ... }:

{
  # nix-darwin configurations
  parts.darwinConfigurations.riscake = {
    system = "aarch64-darwin";
    stateVersion = 4; # only change this if you know what you are doing.
    modules = [ ./riscake/configuration.nix ];
  };

  # NixOS configurations
  parts.nixosConfigurations = {
    mistral = {
      system = "x86_64-linux";
      stateVersion = "23.05";
      server = true;

      modules = [
        inputs.agenix.nixosModules.default
        inputs.crowdsec.nixosModules.crowdsec
        inputs.crowdsec.nixosModules.crowdsec-firewall-bouncer
        inputs.nix-minecraft.nixosModules.minecraft-servers
        self.nixosModules.shpool

        {
          age.secrets = {
            bouncer = {
              file = ../secrets/bouncer.age;
              owner = "moni";
              mode = "0444";
            };

            crowdsec = {
              file = ../secrets/crowdsec.age;
              owner = "moni";
              mode = "0444";
            };
          };
        }

        ./mistral/configuration.nix
      ];
    };

    starcruiser = {
      system = "x86_64-linux";
      stateVersion = "22.11"; # only change this if you know what you are doing.

      modules = [
        inputs.agenix.nixosModules.default

        {
          # NOTE: you should either change this or disable it completely by commenting it out
          age.secrets.tokens = {
            file = ../secrets/tokens.age;
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
      wsl = true;

      modules = [
        inputs.agenix.nixosModules.default

        {
          # NOTE: you should either change this or disable it completely by commenting it out
          age = {
            identityPaths = [ "/home/zero/.ssh/id_ed25519" ];

            secrets.tokens = {
              file = ../secrets/tokens.age;
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
