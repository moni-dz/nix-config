final: prev: {
  alacritty = prev.alacritty.overrideAttrs (old: rec {
    src = prev.fetchFromGitHub {
      owner = "zenixls2";
      repo = "alacritty";
      rev = "df24940a9ccf7ba897fa3167046ee8b181342d8f";
      sha256 = "sha256-2Th2aojTN36MgYSFXiACcBkTpTou/X1Ub5JR2sgZa34=";
    };
    doCheck = false; # NOTE: don't compile twice
    installPhase = ''
      runHook preInstall
      install -D $releaseDir/alacritty $out/bin/alacritty
      install -D extra/linux/Alacritty.desktop -t $out/share/applications/
      install -D extra/logo/compat/alacritty-term.svg $out/share/icons/hicolor/scalable/apps/Alacritty.svg
      strip -S $out/bin/alacritty
      patchelf --set-rpath "${
        prev.lib.makeLibraryPath old.buildInputs
      }:${prev.stdenv.cc.cc.lib}/lib${
        prev.lib.optionalString prev.stdenv.is64bit "64"
      }" $out/bin/alacritty
      installShellCompletion --zsh extra/completions/_alacritty
      installShellCompletion --bash extra/completions/alacritty.bash
      installShellCompletion --fish extra/completions/alacritty.fish
      install -dm 755 "$out/share/man/man1"
      gzip -c extra/alacritty.man > "$out/share/man/man1/alacritty.1.gz"
      install -Dm 644 alacritty.yml $out/share/doc/alacritty.yml
      install -dm 755 "$terminfo/share/terminfo/a/"
      tic -xe alacritty,alacritty-direct -o "$terminfo/share/terminfo" extra/alacritty.info
      mkdir -p $out/nix-support
      echo "$terminfo" >> $out/nix-support/propagated-user-env-packages
      runHook postInstall
    '';
    cargoDeps = old.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "sha256-XMvLUAL25X1CL70EL0ziyhsK9fz6N1XGQ5aLyAeEWkk=";
    });
  });
}
