{
  self,
  inputs,
  ...
}:

let
  pkgsFrom =
    system:
    import inputs.nixpkgs {
      inherit system;
      allowUnfree = true;
      allowUnsupportedSystem = true;
      overlays = [ self.overlays.default ];
    };
in
{
  imports = [ ./overlays.nix ];

  systems = [
    "x86_64-linux"
    "aarch64-darwin"
  ];

  perSystem =
    {
      pkgs,
      system,
      inputs',
      ...
    }:
    {
      _module.args.pkgs = pkgsFrom system;
      packages = removeAttrs (self.overlays.default pkgs pkgs) [ "lib" ] // {
        inherit (inputs'.nixpkgs-f2k.packages) iosevka-ft-bin iosevka-ft-qp-bin;

        inherit (inputs'.nvim.packages) neovim;
      };
    };

  flake.packages = {
    "x86_64-linux" = {
      inherit (inputs.nixpkgs-f2k.packages."x86_64-linux") phocus-modified;

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

    "aarch64-darwin" = {
      inherit (inputs.nixpkgs-f2k.packages."aarch64-darwin") man-pages-xnu;
    };
  };
}
