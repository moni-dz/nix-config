{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";

  inputs = {
    # Non-flake inputs
    impatient-nvim = { url = "github:lewis6991/impatient.nvim"; flake = false; };
    packer-nvim = { url = "github:wbthomason/packer.nvim"; flake = false; };
    sway-borders = { url = "github:fluix-dev/sway-borders"; flake = false; };
    zsh-f-sy-h = { url = "github:zdharma-continuum/fast-syntax-highlighting"; flake = false; };

    # Flake inputs
    agenix.url = "github:ryantm/agenix";
    discocss.url = "github:mlvzk/discocss/flake";
    emacs.url = "github:nix-community/emacs-overlay";
    home.url = "github:nix-community/home-manager";
    neovim.url = "github:neovim/neovim?dir=contrib";
    nix.url = "github:nixos/nix";
    nix-colors.url = "github:Misterio77/nix-colors";
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";

    # Nixpkgs branches
    master.url = "github:nixos/nixpkgs/d6052a689807cf045cb13724f184a27e18583c89";
    stable.url = "github:nixos/nixpkgs/nixos-21.11";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Default Nixpkgs for packages and modules
    nixpkgs.follows = "master";

    # Make every input use one instance of nixpkgs
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    discocss.inputs.nixpkgs.follows = "nixpkgs";
    emacs.inputs.nixpkgs.follows = "nixpkgs";
    home.inputs.nixpkgs.follows = "nixpkgs";
    neovim.inputs.nixpkgs.follows = "nixpkgs";
    nix.inputs.nixpkgs.follows = "nixpkgs";
    nix-colors.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs-f2k.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, agenix, home, nixpkgs, discocss, nix-colors, ... }@inputs:
    let
      config = {
        allowBroken = true;
        allowUnfree = true;
        tarball-ttl = 0;
        /*
          NOTE: experimental option, disable if you don't know what this does

          See https://github.com/NixOS/rfcs/pull/62 for more information.
        */
        contentAddressedByDefault = false;
      };

      filterNixFiles = k: v: v == "regular" && nixpkgs.lib.hasSuffix ".nix" k;

      importNixFiles = path: with nixpkgs.lib; (lists.forEach (mapAttrsToList (name: _: path + ("/" + name))
        (filterAttrs filterNixFiles (builtins.readDir path)))) import;

      overlays = with inputs; [
        (final: _:
          let system = final.system; in
          {
            /*
              Nixpkgs branches

              One can access these branches like so:

              `pkgs.stable.mpd'
              `pkgs.master.linuxPackages_xanmod'
            */
            master = import master { inherit config system; };
            unstable = import unstable { inherit config system; };
            stable = import stable { inherit config system; };
          })

        # Overlays provided by inputs
        emacs.overlay
      ]
      # Overlays from ./overlays directory
      ++ (importNixFiles ./overlays);
    in
    {
      nixosConfigurations.superfluous = import ./hosts/superfluous {
        inherit config nixpkgs agenix overlays inputs;
      };

      homeConfigurations.fortuneteller2k = import ./users/fortuneteller2k {
        inherit config nixpkgs home discocss nix-colors overlays inputs;
      };
    };
}
