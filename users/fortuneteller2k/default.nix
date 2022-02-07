{ config, nixpkgs, home, discocss, nix-colors, nixvim, overlays, inputs }:

home.lib.homeManagerConfiguration rec {
  system = "x86_64-linux";
  username = "fortuneteller2k";
  homeDirectory = "/home/${username}";
  configuration.imports = [ ./home.nix ];
  pkgs = nixpkgs.outputs.legacyPackages.${system};

  extraModules = [
    discocss.hmModule
    nix-colors.homeManagerModule
    nixvim.homeManagerModules.nixvim

    { nixpkgs = { inherit config overlays; }; }
  ];

  extraSpecialArgs = { inherit inputs; };
  stateVersion = "21.05";
}
