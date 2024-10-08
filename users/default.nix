{
  config,
  lib,
  inputs,
  ...
}:

{
  # home-manager configurations
  parts.homeConfigurations = {
    "moni@riscake" = {
      system = "aarch64-darwin";
      stateVersion = "23.05";
      agenix = true;

      modules = [
        ./moni/home.nix
      ] ++ lib.optional config.parts.homeConfigurations."moni@riscake".agenix ./moni/age.nix;
    };

    "moni@starcruiser" = {
      system = "x86_64-linux";
      stateVersion = "21.11";

      modules = [
        inputs.nix-colors.homeManagerModule
        ./omni/home.nix
      ];
    };

    "moni@mistral" = {
      system = "x86_64-linux";
      stateVersion = "23.05";
      modules = [ ./volta/home.nix ];
    };

    "zero@turncoat" = {
      system = "x86_64-linux";
      stateVersion = "21.11";
      modules = [ ./zero/home.nix ];
    };
  };
}
