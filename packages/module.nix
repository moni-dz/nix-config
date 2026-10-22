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
      packages =
        removeAttrs (self.overlays.default pkgs pkgs) [
          "infuse"
          "lib"
        ]
        // {
          inherit (inputs'.nixpkgs-f2k.packages) iosevka-ft-bin iosevka-ft-qp-bin;
          inherit (inputs'.jj.packages) jujutsu;
        };
    };

  flake.packages = {
    "aarch64-darwin" = {
      inherit (inputs.nixpkgs-f2k.packages."aarch64-darwin") man-pages-xnu;
    };
  };
}
