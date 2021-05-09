{ config, agenix, home, inputs, nixpkgs, user-overlays, ... }:

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = [
    agenix.nixosModules.age
    home.nixosModules.home-manager
    nixpkgs.nixosModules.notDetected

    {
      age.secrets.github-token = {
        file = ../../secrets/github-token.age;
        owner = "fortuneteller2k";
        mode = "0444";
      };

      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users.fortuneteller2k = import ../../users/fortuneteller2k;
      };

      nix = import ../../config/nix-conf.nix { inherit inputs system nixpkgs; };

      nixpkgs =
        let
          input-overlays = _: _: with inputs; {
            agenix = agenix.defaultPackage.${system};
            manix = manix.defaultPackage.${system};
            neovim-nightly = neovim.packages.${system}.neovim;
            nix-eval-lsp = nix-eval-lsp.defaultPackage.${system};
          };

          nixpkgs-overlays = _: _: with inputs; {
            master = import master { inherit config system; };
            unstable = import unstable { inherit config system; };
            stable = import stable { inherit config system; };
            staging = import staging { inherit config system; };
            staging-next = import staging-next { inherit config system; };

            # NOTE: remove this, if you're not me or a maintainer of the xanmod kernel in nixpkgs
            kernel = import inputs.kernel { inherit config system; };
          };
        in
        {
          inherit config;

          overlays = with inputs; [
            nixpkgs-overlays
            emacs.overlay
            nur.overlay
            rust.overlay
            input-overlays
          ] ++ user-overlays;
        };
    }

    ./configuration.nix
  ];

  specialArgs = { inherit inputs; };
}
