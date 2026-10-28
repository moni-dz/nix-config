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
          inherit (inputs'.jj.packages) jujutsu;
          nix-src = import ./nix-src.nix { inherit inputs pkgs; };

          neovim = pkgs.callPackage ./neovim {
            neovim = inputs'.neovim-nightly-overlay.packages.default;

            sources = {
              inherit (inputs)
                mellifluous-nvim
                nvim-laurel
                nvim-thyme
                pulse-nvim
                smart-splits-backend-rex
                vellum-nvim
                ;
            };
          };
        };
    };
}
