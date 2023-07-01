{ inputs, ... }:

{
  parts.nixosConfigurations.turncoat = {
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
            file = ../../secrets/github-token.age;
            owner = "zero";
            mode = "0444";
          };
        };

        wsl = {
          enable = true;
          defaultUser = "zero";
          startMenuLaunchers = true;

          wslConf = {
            network.hostname = "turncoat";
            automount.root = "/mnt";
          };
        };
      }

      ./configuration.nix
    ];
  };
}
