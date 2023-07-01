{ inputs, ... }:

{
  parts.nixosConfigurations.starcruiser = {
    system = "x86_64-linux";
    stateVersion = "22.11"; # only change this if you know what you are doing.

    modules = [
      inputs.agenix.nixosModules.default

      {
        # NOTE: you should either change this or disable it completely by commenting it out
        age.secrets.github-token = {
          file = ../../secrets/github-token.age;
          owner = "moni";
          mode = "0444";
        };
      }

      ./configuration.nix
    ];
  };
}
