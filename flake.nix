{
  description = "A somewhat huge NixOS configuration using Nix Flakes.";

  inputs = {
    # Non-flake inputs
    swaywm = { url = "github:swaywm/sway"; flake = false; };
    zsh-f-sy-h = { url = "github:zdharma-continuum/fast-syntax-highlighting"; flake = false; };

    # Flake inputs
    agenix.url = "github:ryantm/agenix";
    emacs.url = "github:nix-community/emacs-overlay";
    doom.url = "github:nix-community/nix-doom-emacs";
    ff-addons.url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
    home.url = "github:nix-community/home-manager";
    hyprland.url = "github:hyprwm/hyprland";
    nix.url = "github:nixos/nix";
    nix-colors.url = "github:Misterio77/nix-colors";
    nixos-wsl.url = "github:nix-community/nixos-wsl";
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";
    nixpkgs-fmt.url = "github:nix-community/nixpkgs-fmt";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nixvim.url = "github:pta2002/nixvim";
    statix.url = "github:nerdypepper/statix";

    # Nixpkgs branches
    master.url = "github:nixos/nixpkgs/master";
    stable.url = "github:nixos/nixpkgs/nixos-21.11";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    xanmod.url = "github:fortuneteller2k/nixpkgs/xanmod";
    nvd.url = "github:dacioromero/nixpkgs/master";

    # Default Nixpkgs for packages and modules
    nixpkgs.follows = "master";

    # Minimize duplicate instances of inputs
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    doom.inputs.nixpkgs.follows = "nixpkgs";
    emacs.inputs.nixpkgs.follows = "nixpkgs";
    ff-addons.inputs.nixpkgs.follows = "nixpkgs";
    home.inputs.nixpkgs.follows = "nixpkgs";
    hyprland.inputs.nixpkgs.follows = "nixpkgs";
    nix.inputs.nixpkgs.follows = "nixpkgs";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs-f2k.inputs.nixpkgs.follows = "nixpkgs";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    statix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, home, nixpkgs, ... }@inputs:
    let
      config = {
        allowBroken = true;
        allowUnfree = true;
        allowUnfreePredicate = _: true;
        tarball-ttl = 0;

        # XXX: don't do this kids...
        # replaceStdenv = { pkgs }: pkgs.optimizedV3Stdenv;

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
          let inherit (final) system; in
          {
            sway-unwrapped = (nixpkgs-wayland.packages.${system}.sway-unwrapped.override {
              stdenv = final.optimizedV3Stdenv;
              wlroots_0_16 = hyprland.packages.${system}.wlroots-hyprland.override { nvidiaPatches = true; };
            }).overrideAttrs (_: {
              __contentAddressed = true;
              src = swaywm;
            });

            inherit (nvd.legacyPackages.${system}) nvidia-vaapi-driver;

            /*
              Nixpkgs branches, replace when https://github.com/NixOS/nixpkgs/pull/160061 is live.

              One can access these branches like so:

              `pkgs.stable.mpd'
              `pkgs.master.linuxPackages_xanmod'
            */
            master = import master { inherit config system; };
            unstable = import unstable { inherit config system; };
            stable = import stable { inherit config system; };
            xanmod = import xanmod { inherit config system; };
          })

        # Overlays provided by inputs
        emacs.overlay
        inputs.nixpkgs-f2k.overlays.stdenvs
      ]
      # Overlays from ./overlays directory
      ++ (importNixFiles ./overlays);
    in
    {
      nixosConfigurations = {
        superfluous = import ./hosts/superfluous {
          inherit config nixpkgs overlays inputs;
        };

        starcruiser = import ./hosts/starcruiser {
          inherit config nixpkgs overlays inputs;
        };

        turncoat = import ./hosts/turncoat {
          inherit config nixpkgs overlays inputs;
        };
      };

      homeConfigurations = {
        moni = import ./users/moni {
          inherit config nixpkgs home overlays inputs;
        };

        zero = import ./users/zero {
          inherit config nixpkgs home overlays inputs;
        };
      };

      # Easier `nix build`-ing of configurations
      superfluous = self.nixosConfigurations.superfluous.config.system.build.toplevel;
      starcruiser = self.nixosConfigurations.starcruiser.config.system.build.toplevel;
      turncoat = self.nixosConfigurations.turncoat.config.system.build.toplevel;

      # Default formatter for the entire repo
      formatter = nixpkgs.lib.genAttrs [ "x86_64-linux" ] (system: inputs.nixpkgs-fmt.defaultPackage.${system});
    };

  nixConfig = {
    commit-lockfile-summary = "flake: bump inputs";

    substituters = [
      "https://cache.nixos.org?priority=10"
      "https://cache.ngi0.nixos.org/"
      "https://nix-community.cachix.org?priority=5"
      "https://nixpkgs-wayland.cachix.org"
      "https://fortuneteller2k.cachix.org"
    ];
  };
}
