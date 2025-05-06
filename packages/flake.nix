{
  description = "Stub flake for drv partition dependencies";
  nixConfig.commit-lockfile-summary = "flake: bump package inputs";
  outputs = _: { };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    nixpkgs-f2k.url = "github:moni-dz/nixpkgs-f2k";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nvim.url = "github:nix-community/neovim-nightly-overlay";
    jj.url = "github:jj-vcs/jj";
    infuse = { url = "git+https://codeberg.org/amjoseph/infuse.nix.git"; flake = false; };

    nixpkgs-f2k.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";
    nvim.inputs.nixpkgs.follows = "nixpkgs";
    jj.inputs.nixpkgs.follows = "nixpkgs";
  };
}
