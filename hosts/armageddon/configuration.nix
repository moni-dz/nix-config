{ config, lib, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = lib.attrValues {
    inherit (pkgs) discord git vim;
  };

  # Auto upgrade nix package and the daemon service.
  services = {
    nix-daemon.enable = true;
  };

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  security.pam.enableSudoTouchIdAuth = true;

  homebrew = {
    enable = true;
    
    onActivation = {
      autoUpdate = true;
      upgrade = true;
    };

    casks = [
      "amethyst"
      "raycast"
    ];
  };



  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
