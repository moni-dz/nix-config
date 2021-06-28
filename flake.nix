{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";

  inputs = {
    # Non-flake inputs
    nixos-wallpapers = { url = "github:fortuneteller2k/nixos-wallpapers"; flake = false; };
    phocus = { url = "github:fortuneteller2k/gtk"; flake = false; };
    twobwm = { url = "github:venam/2bwm"; flake = false; };
    vim-flowtune = { url = "github:fortuneteller2k/vim-flowtune"; flake = false; };
    vim-horizon = { url = "github:fortuneteller2k/vim-horizon"; flake = false; };
    zsh-doas = { url = "github:anatolykopyl/doas-zsh-plugin"; flake = false; };
    zsh-f-sy-h = { url = "github:zdharma/fast-syntax-highlighting"; flake = false; };

    # Flake inputs
    agenix.url = "github:ryantm/agenix";
    emacs.url = "github:nix-community/emacs-overlay";
    emacs-ng.url = "github:emacs-ng/emacs-ng";
    home.url = "github:nix-community/home-manager";
    manix.url = "github:mlvzk/manix";
    neovim.url = "github:neovim/neovim?dir=contrib";
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";
    review.url = "github:Mic92/nixpkgs-review";
    rust.url = "github:oxalica/rust-overlay";

    # Nixpkgs branches
    master.url = "github:nixos/nixpkgs/master";
    stable.url = "github:nixos/nixpkgs/nixos-21.05";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    /*
      NOTE: don't use this, if you're not me or a maintainer of the XanMod kernel in Nixpkgs

      I nuke this branch from time to time.
    */
    # kernel.url = "github:samuelgrf/nixpkgs/xanmod-features-kconfig";

    # Default Nixpkgs for packages and modules
    nixpkgs.follows = "master";
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

      overlays = with inputs; [
        (final: _:
          let
            system = final.system;
          in
          {
            # Packages provided by flake inputs
            agenix = agenix.defaultPackage.${system};
            awesome = nixpkgs-f2k.packages.${system}.awesome-git;
            picom = nixpkgs-f2k.packages.${system}.picom-git;
            slock = nixpkgs-f2k.packages.${system}.slock-fancy;
            iosevka-ft-bin = nixpkgs-f2k.packages.${system}.iosevka;
            manix = manix.defaultPackage.${system};
            neovim-nightly = neovim.packages.${system}.neovim;
            nixpkgs-review = review.defaultPackage.${system};
            emacsNg = emacs-ng.defaultPackage.${system};

            # Sources provided by non-flake inputs, to be used in overlays and derivations
            inherit nixos-wallpapers zsh-doas zsh-f-sy-h;
            phocus-src = phocus;
            twobwm-src = twobwm;
            vim-flowtune-src = vim-flowtune;
            vim-horizon-src = vim-horizon;

            /*
              Nixpkgs branches

              One can access these branches like so:

              `pkgs.stable.mpd'
              `pkgs.master.linuxPackages_xanmod'
            */
            master = import master { inherit config system; };
            unstable = import unstable { inherit config system; };
            stable = import stable { inherit config system; };

            # NOTE: Remove this, if you're not me or a maintainer of the XanMod kernel in Nixpkgs
            # kernel = import inputs.kernel { inherit config system; };
          })

        # Overlays provided by inputs
        emacs.overlay
        rust.overlay
        nixpkgs-f2k.overlay
      ]
      # Overlays from ./overlays directory
      ++ (importNixFiles ./overlays);
    in
    {
      nixosConfigurations.superfluous = import ./hosts/superfluous {
        inherit config agenix home inputs nixpkgs overlays;
      };

      superfluous = self.nixosConfigurations.superfluous.config.system.build.toplevel;
    };
}
