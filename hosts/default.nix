{ inputs, ... }:

{
  # nix-darwin configurations
  parts.darwinConfigurations.riscake = {
    system = "aarch64-darwin";
    stateVersion = 5; # only change this if you know what you are doing.
    determinate = true; # use Determinate Nix
    modules = [
      inputs.agenix.darwinModules.default
      inputs.hjem.darwinModules.default
      inputs.nix-index-database.darwinModules.nix-index
      ./riscake/configuration.nix
      ../users/moni
    ];
  };

  # NixOS configurations
  parts.nixosConfigurations = {
    # Steam Deck (Jovian-NixOS)
    hands = {
      system = "x86_64-linux";
      stateVersion = "26.05";
      modules = [
        inputs.agenix.nixosModules.default
        inputs.jovian.nixosModules.default
        inputs.rex.nixosModules.default
        ./hands/configuration.nix
      ];
    };
  };
}
