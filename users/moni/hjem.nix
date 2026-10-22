{
  config,
  pkgs,
  osConfig,
  self',
  ...
}:

{
  imports = [ ../../profiles/hjem ];

  packages = __attrValues {
    inherit (pkgs) iproute2mac watchman;
  };

  rum.programs = {
    jujutsu.package = self'.packages.jujutsu;

    nushell.envFile = ''
      open ${osConfig.age.secrets.tokens.path} | split column " " | get column1 | split column "=" | reduce -f {} {|it, acc| $acc | upsert $it.column0 $it.column1 } | load-env
      $env.BUN_INSTALL = ($env.HOME | path join ".bun")
      $env.path ++= ["/etc/profiles/per-user/${config.user}/bin" "/run/current-system/sw/bin" "/usr/bin" "/usr/local/bin" "/opt/homebrew/bin" "${config.directory}/.modular/bin" "/opt/local/bin" "/opt/homebrew/opt/llvm/bin" "${config.directory}/.local/share/modular/pkg/packages.modular.com_mojo/bin" ($env.BUN_INSTALL | path join "bin") "/Users/Shared/DBngin/postgresql/17.0/bin" "${config.directory}/.local/bin" "${config.directory}/.cargo/bin" "/Applications/OrbStack.app/Contents/MacOS/bin" "/Applications/OrbStack.app/Contents/MacOS/xbin" "/nix/var/nix/profiles/default/bin"]
    '';
  };
}
