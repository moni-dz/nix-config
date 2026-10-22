{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    getExe
    mkAfter
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    mkPackageOption
    ;

  toml = pkgs.formats.toml { };
  cfg = config.rum.programs.jujutsu;
  gitStatus = config.rum.programs.starship.settings.git_status;
  jj-log = "jj log -r@ -n1 --no-graph --ignore-working-copy --color never -T";
in
{
  options.rum.programs.jujutsu = {
    enable = mkEnableOption "jujutsu";
    package = mkPackageOption pkgs "jujutsu" { };

    settings = mkOption {
      inherit (toml) type;
      default = { };
      description = "Configuration written to {file}`$XDG_CONFIG_HOME/jj/config.toml`.";
    };

    integrations = {
      nushell.enable = mkEnableOption "jujutsu completions for nushell";
      starship.enable = mkEnableOption "jujutsu segments for starship";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      packages = [ cfg.package ];

      xdg.config.files."jj/config.toml" = mkIf (cfg.settings != { }) {
        source = toml.generate "jj-config.toml" cfg.settings;
      };

      rum.programs.nushell.extraConfig = mkIf cfg.integrations.nushell.enable (mkAfter ''
        source ${
          pkgs.runCommand "jj-completion-nu" { } ''${getExe cfg.package} util completion nushell > "$out"''
        }
      '');
    }

    (mkIf cfg.integrations.starship.enable {
      rum.programs.starship.settings.custom = {
        jjid = {
          ignore_timeout = true;
          description = "current jj revision";
          when = "jj root";
          command = "${jj-log} 'change_id.shortest(4)'";
          style = "fg:black bg:green";
          format = "[ $output ]($style)";
        };

        jjbookmark = {
          ignore_timeout = true;
          description = "jj bookmark of current revision";
          when = "jj root";
          command = "${jj-log} 'self.bookmarks()' | tr ' ' '\n  ' | sort -k1.1n | head -1";
          style = "fg:black bg:purple";
          format = "[( $output )]($style)";
        };

        jjstat = {
          ignore_timeout = true;
          description = "current jj bookmark status";
          when = "jj root";
          style = "fg:black bg:red";
          format = "[( $output )]($style)";
          command = ''
            ${jj-log} '
              separate("",
                if(!empty, "∆"),
                if(conflict, "${gitStatus.conflicted}"),
                if(divergent, "${gitStatus.diverged}"),
                if(hidden, "${gitStatus.deleted}"),
                if(immutable, "λ"),
                if(self.contained_in("first_parent(@)"), "★"),
                if(root, "☆")
              )
            '
          '';
        };
      };
    })
  ]);
}
