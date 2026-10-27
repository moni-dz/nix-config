{ inputs, pkgs }:

let
  nix-src = import inputs.nix-src.inputs.nixpkgs {
    localSystem = pkgs.stdenv.hostPlatform.system;
    overlays = [
      (_: prev: { libgit2 = prev.libgit2.overrideAttrs { doCheck = false; }; })
      inputs.nix-src.overlays.internal
    ];
  };

  inherit (nix-src) lib;
  inherit (nix-src.stdenv.hostPlatform) isLinux;

  stdenv =
    if isLinux then nix-src.stdenvAdapters.useMoldLinker nix-src.clangStdenv else nix-src.stdenv;

  components =
    (nix-src.nixComponents2.overrideScope (
      _: prev:
      lib.optionalAttrs isLinux { inherit stdenv; }
      // {
        # unity builds merge every component into one giant translation unit (`unity_size=8192`),
        # which saves total cpu time but compiles on a single core
        withUnityBuild = false;

        nix-cli = prev.nix-cli.overrideAttrs (old: {
          # an example command line in `nix`'s own docs embeds the compiler's store path,
          # which drags all of the compiler into the closure of anything that has nix
          postFixup = (old.postFixup or "") + ''
            ${nix-src.removeReferencesTo}/bin/remove-references-to \
              -t ${nix-src.stdenv.cc.cc} -t ${stdenv.cc.cc} $out/bin/nix
          '';
        });
      }
    )).overrideAllMesonComponents
      (
        _: prev: {
          # every release build turns on `b_lto`, which with clang is full LTO: the whole
          # link is one single-threaded pass, thin LTO spreads it over all the cores
          preConfigure =
            (prev.preConfigure or "")
            + lib.optionalString isLinux ''
              appendToVar mesonFlags -Db_lto_mode=thin -Db_lto_threads=$NIX_BUILD_CORES
            '';
        }
      );
in
pkgs.lib.infuse components.nix-everything {
  __output.doCheck.__assign = false;
}
