{
  self,
  inputs,
  lib,
  ...
}:

let
  pkgsFrom =
    system:
    import inputs.nixpkgs {
      inherit system;
      allowUnfree = true;
      allowUnsupportedSystem = true;
      overlays = [
        self.overlays.default
        inputs.emacs.overlay
      ];
    };
in
{
  imports = [ ./overlays.nix ];
  systems = [
    "x86_64-linux"
    "aarch64-darwin"
  ];

  perSystem =
    { pkgs, system, ... }:
    {
      _module.args.pkgs = pkgsFrom system;
      packages = removeAttrs (self.overlays.default pkgs pkgs) [ "lib" ];
    };

  flake.packages = {
    "x86_64-linux" =
      let
        pkgs = pkgsFrom "x86_64-linux";
      in
      removeAttrs (inputs.nixpkgs-f2k.overlays.linux pkgs pkgs) [ "lib" ]
      // {
        inherit (inputs.nixpkgs-wayland.packages."x86_64-linux")
          dunst
          waybar
          grim
          slurp
          swaybg
          swayidle
          swaylock
          wf-recorder
          wl-clipboard
          wlogout
          ;
      };
    "aarch64-darwin" =
      let
        pkgs = pkgsFrom "aarch64-darwin";
      in
      removeAttrs (inputs.nixpkgs-f2k.overlays.darwin pkgs pkgs) [ "lib" ];
  };
}
