{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";

  inputs = {
    # Non-flake inputs
    phocus = { url = "github:fortuneteller2k/gtk"; flake = false; };
    picom = { url = "github:yshui/picom"; flake = false; };
    slock = { url = "github:khuedoan/slock"; flake = false; };
    twobwm = { url = "github:venam/2bwm"; flake = false; };
    vim-flowtune = { url = "github:fortuneteller2k/vim-flowtune"; flake = false; };
    vim-horizon = { url = "github:fortuneteller2k/vim-horizon"; flake = false; };
    xmonad = { url = "github:xmonad/xmonad"; flake = false; };
    xmonad-contrib = { url = "github:xmonad/xmonad-contrib"; flake = false; };

    # Flake inputs
    agenix.url = "github:ryantm/agenix";
    emacs.url = "github:nix-community/emacs-overlay";
    emacs-ng.url = "github:emacs-ng/emacs-ng";
    home.url = "github:nix-community/home-manager";
    manix.url = "github:mlvzk/manix";
    neovim.url = "github:neovim/neovim?dir=contrib";
    nur.url = "github:nix-community/NUR/13d1f3087d69e7ea6845dab0af7c77bb3ad3af53";
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
    # kernel.url = "github:fortuneteller2k/nixpkgs/update-xanmod-512";
    usbmuxd.url = "github:fortuneteller2k/nixpkgs/usbmuxd";

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
            system = final.stdenv.hostPlatform.system;
          in
          {
            # Packages provided by flake inputs
            agenix = agenix.defaultPackage.${system};
            manix = manix.defaultPackage.${system};
            neovim-nightly = neovim.packages.${system}.neovim;
            nixpkgs-review = review.defaultPackage.${system};
            emacsNg = emacs-ng.defaultPackage.${system};

            # Sources provided by non-flake inputs, to be used in overlays and derivations
            phocus-src = phocus;
            picom-src = picom;
            slock-src = slock;
            twobwm-src = twobwm;
            vim-flowtune-src = vim-flowtune;
            vim-horizon-src = vim-horizon;
            xmonad-src = xmonad;
            xmonad-contrib-src = xmonad-contrib;

            /*
              Nixpkgs branches

              One can access these branches like so:

              `pkgs.stable.mpd'
              `pkgs.master.linuxPackages_xanmod'
            */
            master = import master { inherit config system; };
            unstable = import unstable { inherit config system; };
            stable = import stable { inherit config system; };
            muxd = import usbmuxd { inherit config system; };

            # NOTE: Remove this, if you're not me or a maintainer of the XanMod kernel in Nixpkgs
            kernel = import inputs.kernel { inherit config system; };
          })

        # Overlays provided by inputs
        emacs.overlay
        nur.overlay
        rust.overlay
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
