{
  config,
  lib,
  pkgs,
  inputs',
  infuse,
  ...
}:

{
  imports = [ ./hardware-configuration.nix ];

  age.secrets.netrc = {
    file = ../../secrets/netrc.age;
    mode = "444";
  };

  nix.settings = {
    netrc-file = config.age.secrets.netrc.path;
    extra-trusted-users = [ "lyt" ];
  };

  nixpkgs.overlays = [
    (_: prev: {
      jovianPackages = prev.jovianPackages.overrideScope (
        _: super: {
          gamescope = super.gamescope.overrideAttrs (old: {
            patches = builtins.filter (
              p:
              !(builtins.any (s: prev.lib.hasSuffix s (toString p)) [
                # wlroots libinput backport, already in 3.16.30.1
                "c08d99437ec8bb56a703f04ad1ef199502c62d10.diff"
              ])
            ) old.patches;

            # unit tests need catch2
            mesonFlags = old.mesonFlags ++ [ (prev.lib.mesonBool "enable_tests" false) ];
          });
        }
      );
    })
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  networking.networkmanager = {
    enable = true;
    dns = "systemd-resolved";
  };

  time.timeZone = "Asia/Manila";
  i18n.defaultLocale = "en_US.UTF-8";

  services = {
    desktopManager.plasma6.enable = true;
    openssh.enable = true;
    resolved.enable = true;
    tailscale.enable = true;

    displayManager.sddm = {
      enable = true;
      wayland.enable = true;
    };

    rex.loginServer = {
      enable = true;
      accounts.lyt.principals = [ "tailscale:moni-dz@github" ];
      tailscaleServe.enable = true;
    };
  };

  jovian.steam = {
    enable = true;
    autoStart = true;
    user = "lyt";
    desktopSession = "plasma";
  };

  users.users.lyt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    packages = [ pkgs.nano ];
  };

  programs.bash.interactiveShellInit = ''
    if [ "$(id -u)" != 0 ] && [ -z "$IN_NUSHELL" ]; then
      export IN_NUSHELL=1
      exec ${lib.getExe pkgs.nushell}
    fi
  '';

  environment.variables.EDITOR = "nvim";

  /*
    rex is built on some older nixpkgs commit, using follows will make it segfault
    it links libpam on runtime, so running a newer nixpkgs commit will cause it to segfault
    without manually rewriting the RPATH
  */
  services.rex.package = infuse inputs'.rex.packages.rex {
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

  hjem.users.lyt = {
    enable = true;
    imports = [ ../../users/common.nix ];
    rum.programs.jujutsu.settings = {
      fsmonitor.backend = lib.mkForce "none";
      signing.behavior = lib.mkForce "drop";
    };

    rum.programs.nushell = {
      envFile = ''
        $env.path ++= ["/run/wrappers/bin" "/etc/profiles/per-user/lyt/bin" "/run/current-system/sw/bin" "/nix/var/nix/profiles/default/bin"]
      '';
    };
  };
}
