{ inputs, system, nixpkgs, max-jobs }:

# Nix daemon settings that can't be put in `nixConfig`.
{
  daemonCPUSchedPolicy = "idle";
  daemonIOSchedClass = "idle";
  daemonIOSchedPriority = 5;

  nixPath =
    let path = toString ./.;
    in
    [
      "repl=${path}/repl.nix"
      "nixpkgs=${nixpkgs}"
      "home-manager=${inputs.home}"
    ];

  package = inputs.master.legacyPackages.${system}.nix;

  registry = {
    system.flake = inputs.self;
    default.flake = nixpkgs;
    home-manager.flake = inputs.home;
  };

  settings.max-jobs = max-jobs;
}
