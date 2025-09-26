{
  config,
  lib,
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
      ]
      ++ lib.optional config.parts.homeConfigurations."moni@riscake".agenix ./moni/age.nix;
    };

    "moni@weasel" = {
      system = "x86_64-linux";
      stateVersion = "24.11";
      agenix = true;
      modules = [
        ./zero/home.nix
      ]
      ++ lib.optional config.parts.homeConfigurations."moni@weasel".agenix ./moni/age.nix;
    };
  };
}
