{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";

  inputs = {
    # Non-flake inputs
    phocus = { url = "github:fortuneteller2k/gtk"; flake = false; };
    material-nvim-src = { url = "github:marko-cerovac/material.nvim"; flake = false; };
    zsh-doas = { url = "github:anatolykopyl/doas-zsh-plugin"; flake = false; };
    zsh-f-sy-h = { url = "github:zdharma-continuum/fast-syntax-highlighting"; flake = false; };

    # Flake inputs
    discocss.url = "github:mlvzk/discocss/flake";
    emacs.url = "github:nix-community/emacs-overlay";
    home.url = "github:nix-community/home-manager";
    neovim.url = "github:neovim/neovim?dir=contrib";
    nixvim.url = "github:pta2002/nixvim";
    nix-colors.url = "github:Misterio77/nix-colors";
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    ragenix.url = "github:yaxitech/ragenix";

    # Nixpkgs branches
    master.url = "github:nixos/nixpkgs";
    stable.url = "github:nixos/nixpkgs/nixos-21.11";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Default Nixpkgs for packages and modules
    nixpkgs.follows = "master";
  };

  outputs = { self, ragenix, home, nixpkgs, ... } @ inputs:
    with nixpkgs.lib;
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
            ragenix = ragenix.defaultPackage.${system};
            neovim-nightly = neovim.packages.${system}.neovim;
          }
          //
          (with nixpkgs-f2k.packages.${system}; {
            alacritty = alacritty-ligatures;
            iosevka-ft-bin = iosevka;
            river = river-git;
            # sway-unwrapped = sway-borders;
          })
          //
          (with nixpkgs-wayland.packages.${system}; {
            inherit sway-unwrapped swayidle swaylock slurp oguri waybar wf-recorder xdg-desktop-portal-wlr;
          })
          //
          {
            # Sources provided by non-flake inputs, to be used in overlays and derivations
            inherit material-nvim-src zsh-doas zsh-f-sy-h;
            phocus-src = phocus;

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
        nixpkgs-f2k.overlay
      ]
      # Overlays from ./overlays directory
      ++ (importNixFiles ./overlays);
    in
    {
      nixosConfigurations.superfluous = import ./hosts/superfluous {
        inherit config ragenix home inputs nixpkgs overlays;
      };

      superfluous = self.nixosConfigurations.superfluous.config.system.build.toplevel;
    };
}
