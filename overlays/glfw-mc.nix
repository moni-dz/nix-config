final: prev: {
  glfw-wayland = prev.glfw-wayland.overrideAttrs (old: {
    patches = [
      (prev.fetchpatch {
        url = "https://raw.githubusercontent.com/NixOS/nixpkgs/nixos-unstable/pkgs/development/libraries/glfw/wayland.patch";
        hash = "sha256-+oA9eS5Vd/gE/wQUTyJAoUTKSoeDgLTSFLHez7vDWwU=";
      })

      (prev.fetchpatch {
        url = "https://github.com/Admicos/minecraft-wayland/commit/ceda97a3f0717050f27093ed2b6682b29415e8d1.patch";
        hash = "sha256-LuzVRiJks+iEZZExRLT79k113eycG/EZmT6rCXgDacc=";
        excludes = [ ".SRCINFO" "PKGBUILD" "README.md" ];
      })
    ];
  });
}
