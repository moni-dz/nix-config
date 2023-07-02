{ config, lib, inputs, ... }:

{
  # home-manager configurations
  parts.homeConfigurations = {
    moni = {
      system = "aarch64-darwin";
      stateVersion = "23.05";
      agenix = true;

      modules = [ ./moni/home.nix ]
        ++ lib.optional config.parts.homeConfigurations.moni.agenix ./moni/age.nix;
    };

    omni = {
      system = "x86_64-linux";
      stateVersion = "21.11";

      modules = [
        inputs.nix-colors.homeManagerModule
        ./omni/home.nix
      ];
    };

    zero = {
      system = "x86_64-linux";
      stateVersion = "21.11";
      modules = [ ./zero/home.nix ];
    };
  };
}
