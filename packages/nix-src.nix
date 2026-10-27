{ inputs, pkgs }:

let
  nix-src = import inputs.nix-src.inputs.nixpkgs {
    localSystem = pkgs.stdenv.hostPlatform.system;
    overlays = [
      (_: prev: { libgit2 = prev.libgit2.overrideAttrs { doCheck = false; }; })
      inputs.nix-src.overlays.internal
    ];
  };

  components = nix-src.nixComponents2.overrideScope (
    _: prev: {
      nix-cli = prev.nix-cli.overrideAttrs (old: {
        # an example command line in `nix`'s own docs embeds the compiler's store path,
        # which drags all of gcc into the closure of anything that has nix
        postFixup = (old.postFixup or "") + ''
          ${nix-src.removeReferencesTo}/bin/remove-references-to -t ${nix-src.stdenv.cc.cc} $out/bin/nix
        '';
      });
    }
  );
in
pkgs.lib.infuse components.nix-everything {
  __output.doCheck.__assign = false;
}
