final: prev: {
  pulseeffects = prev.pulseeffects.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "wwmm";
      repo = "pulseeffects";
      rev = "v5.0.0";
      sha256 = "sha256-soPBiFQqPg9seOdTYqvtkkEm/Kvvhk+JWOzRvuMaQf8=";
    };
    
    buildInputs = (old.buildInputs or [ ]) ++ [ prev.pipewire ];
  });
}
