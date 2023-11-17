{ inputs, modulesPath, lib, pkgs, ... }:

{
  imports = lib.optional (builtins.pathExists ./do-userdata.nix) ./do-userdata.nix ++ [
    (modulesPath + "/virtualisation/digital-ocean-config.nix")
  ];

  nixpkgs.overlays = [ inputs.nix-minecraft.overlay ];

  environment.systemPackages = with pkgs; [ git vim ];
}
