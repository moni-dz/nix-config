{ inputs, ... }:

{
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
            file = ../../secrets/github-token.age;
            owner = "moni";
            group = "staff";
            mode = "600";
          };
        };
      }

      ./configuration.nix
    ];
  };
}
