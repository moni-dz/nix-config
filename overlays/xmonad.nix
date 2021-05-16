final: prev: {
  haskellPackages = prev.haskellPackages.extend (prev.haskell.lib.packageSourceOverrides {
    xmonad = prev.fetchFromGitHub {
      owner = "xmonad";
      repo = "xmonad";
      rev = "a5cee9bac265485d3e85ea707aeeac5e34a94ba3";
      sha256 = "sha256-+/4KpO8DzLSMQ3kBocSfgNn/Nz2YhBUeujpW75rYxSE=";
    };

    xmonad-contrib = prev.fetchFromGitHub {
      owner = "xmonad";
      repo = "xmonad-contrib";
      rev = "bf5dce592fb677e459f32ce0366290149bd7b4ec";
      sha256 = "sha256-idN19eblF7n+LLveHWfieluzV+SVVy5A5v+Yqo14G/E=";
    };
  });
}
