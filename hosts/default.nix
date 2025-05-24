{ self, inputs, ... }:

{
  # nix-darwin configurations
  parts.darwinConfigurations.riscake = {
    system = "aarch64-darwin";
    stateVersion = 5; # only change this if you know what you are doing.
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
        inputs.nix-minecraft.nixosModules.minecraft-servers
        self.nixosModules.shpool

        ./mistral/configuration.nix
      ];
    };

    weasel = {
      system = "x86_64-linux";
      stateVersion = "24.11";
      wsl = true;

      modules = [
        ./weasel/configuration.nix
      ];
    };
  };
}
