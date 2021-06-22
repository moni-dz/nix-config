final: prev: {
  kile-wl = prev.kile-wl.overrideAttrs (old: rec {
    src = final.kile-wl-src;
    cargoPatches = [];

    cargoDeps = old.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "sha256-Wfx0jl2cwdBoDu5S5n3kGkXmfXZp6aIaebNMrWNe8wU=";
      cargoPatches = [];
    });
  });
}
