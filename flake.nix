{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";

  inputs = {
    home = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix.url = "github:ryantm/agenix";
    emacs.url = "github:nix-community/emacs-overlay";
    manix.url = "github:mlvzk/manix";
    neovim.url = "github:nix-community/neovim-nightly-overlay";
    nur.url = "github:nix-community/NUR";
    rust.url = "github:oxalica/rust-overlay";

    # nixpkgs branches
    master.url = "github:nixos/nixpkgs/master";
    stable.url = "github:nixos/nixpkgs/release-20.09";
    staging.url = "github:nixos/nixpkgs/staging";
    staging-next.url = "github:nixos/nixpkgs/staging-next";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # NOTE: don't use this, if you're not me or a maintainer of the xanmod kernel in nixpkgs
    kernel.url = "github:fortuneteller2k/nixpkgs/update-xanmod-512";

    # default nixpkgs for packages and modules
    nixpkgs.follows = "unstable";
  };

  outputs = { self, agenix, home, nixpkgs, ... } @ inputs:
    with nixpkgs.lib;
    let
      config = {
        allowBroken = true;
        allowUnfree = true;
        /*
          NOTE: experimental option, disable if you don't know what this does

          See https://github.com/NixOS/rfcs/pull/62 for more information.
        */
        contentAddressedByDefault = false;
      };

      filterNixFiles = k: v: v == "regular" && hasSuffix ".nix" k;

      importNixFiles = path: (lists.forEach (mapAttrsToList (name: _: path + ("/" + name))
        (filterAttrs filterNixFiles (builtins.readDir path)))) import;

      user-overlays = importNixFiles ./overlays;

      input-overlays = final: _: with inputs;
        let
          system = final.stdenv.hostPlatform.system;
        in
        {
          agenix = agenix.defaultPackage.${system};
          manix = manix.defaultPackage.${system};
          neovim-nightly = neovim.packages.${system}.neovim;
        };

      nixpkgs-overlays = final: _: with inputs;
        let
          system = final.stdenv.hostPlatform.system;
        in
        {
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
      nixosConfigurations.superfluous = import ./hosts/superfluous {
        inherit config agenix home inputs nixpkgs input-overlays nixpkgs-overlays user-overlays;
      };

      superfluous = self.nixosConfigurations.superfluous.config.system.build.toplevel;
    };
}
