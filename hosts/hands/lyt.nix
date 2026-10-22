{ pkgs, ... }:

{
  users.users.lyt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    packages = [ pkgs.nano ];
  };

  hjem.users.lyt = {
    enable = true;
    imports = [ ../../profiles/hjem ];
    rum.programs.jujutsu.settings = {
      fsmonitor.backend = "none";
      signing.behavior = "drop";
    };

    rum.programs.nushell = {
      envFile = ''
        $env.path ++= ["/run/wrappers/bin" "/etc/profiles/per-user/lyt/bin" "/run/current-system/sw/bin" "/nix/var/nix/profiles/default/bin"]
      '';
    };
  };
}
