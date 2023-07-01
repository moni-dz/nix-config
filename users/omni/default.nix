{ inputs, ... }:

{
  parts.homeConfigurations.omni = {
    system = "x86_64-linux";
    stateVersion = "21.11";

    modules = [
      inputs.nix-colors.homeManagerModule
      ./home.nix
    ];
  };
}
