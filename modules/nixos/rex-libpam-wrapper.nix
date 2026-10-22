{
  inputs',
  lib,
  pkgs,
  ...
}:

{
  /*
    rex is built on some older nixpkgs commit, using follows will make it segfault
    it links libpam on runtime, so running a newer nixpkgs commit will cause it to segfault
    without manually rewriting the RPATH
  */
  services.rex.package = pkgs.lib.infuse inputs'.rex.packages.rex {
    __output = {
      nativeBuildInputs =
        old: lib.filter (p: lib.getName p != "auto-patchelf-hook") old ++ [ pkgs.patchelf ];

      postFixup.__assign = ''
        patchelf --set-interpreter ${pkgs.stdenv.cc.bintools.dynamicLinker} $out/bin/rex
        mv $out/bin/rex $out/bin/rex-real
        cat > $out/bin/rex <<'EOF'
        #!/bin/sh
        export LD_LIBRARY_PATH="${lib.makeLibraryPath [ pkgs.pam ]}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
        exec "${placeholder "out"}/bin/rex-real" "$@"
        EOF
        chmod +x $out/bin/rex
      '';
    };
  };
}
