{
  hjem.users.moni = {
    enable = true;
    imports = [
      ../../profiles/hjem
      (
        { osConfig, pkgs, ... }:

        {
          disabledModules = [
            ../../profiles/hjem/typst.nix
            ../../profiles/hjem/yazi.nix
          ];

          rum.programs = {
            jujutsu.settings.fsmonitor.backend = "none";

            neovim.extraPackages = __attrValues {
              inherit (pkgs)
                gitMinimal
                nixd
                nixfmt
                ripgrep
                ;
            };

            nushell.envFile = ''
              open --raw ${osConfig.age.secrets.tokens.path} | lines | parse "export {key}={value}" | reduce -f {} {|it, acc| $acc | upsert $it.key $it.value } | load-env
              $env.path ++= ["/run/wrappers/bin" "/etc/profiles/per-user/moni/bin" "/run/current-system/sw/bin" "/nix/var/nix/profiles/default/bin"]
            '';
          };
        }
      )
    ];
  };
}
