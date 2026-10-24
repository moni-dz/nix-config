{
  description = "Stub flake for drv partition dependencies";
  nixConfig.commit-lockfile-summary = "flake: bump package inputs";
  outputs = _: { };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    jj.url = "github:jj-vcs/jj";
    parts.url = "github:hercules-ci/flake-parts";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    infuse = {
      url = "https://codeberg.org/amjoseph/infuse.nix/archive/364ea18b5611b5fd6a6acd7151411b430a70e194.tar.gz";
      flake = false;
    };

    # Neovim plugins that aren't in nixpkgs
    mellifluous-nvim = {
      url = "github:ramojus/mellifluous.nvim";
      flake = false;
    };

    vellum-nvim = {
      url = "github:blackhat-7/vellum.nvim";
      flake = false;
    };

    pulse-nvim = {
      url = "github:willyelm/pulse.nvim";
      flake = false;
    };

    smart-splits-backend-rex = {
      url = "github:rawnly/smart-splits-backend-rex";
      flake = false;
    };

    jj.inputs.nixpkgs.follows = "nixpkgs";
    neovim-nightly-overlay.inputs.nixpkgs.follows = "nixpkgs";
    neovim-nightly-overlay.inputs.flake-parts.follows = "parts";
  };
}
