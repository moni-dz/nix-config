{
  description = "Stub flake for drv partition dependencies";
  nixConfig.commit-lockfile-summary = "flake: bump package inputs";
  outputs = _: { };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    jj.url = "github:jj-vcs/jj";
    infuse = {
      url = "https://codeberg.org/amjoseph/infuse.nix/archive/364ea18b5611b5fd6a6acd7151411b430a70e194.tar.gz";
      flake = false;
    };

    jj.inputs.nixpkgs.follows = "nixpkgs";
  };
}
