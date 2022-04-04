{ lib, pkgs, config, modulesPath, ... }:

{
  imports = [ "${modulesPath}/profiles/minimal.nix" ];

  environment.systemPackages = with pkgs; [
    git
    home-manager
  ];

  users.users.zero = {
    isNormalUser = true;
    home = "/home/zero";
    shell = pkgs.zsh;
    extraGroups = [ "wheel" ];
  };
}
