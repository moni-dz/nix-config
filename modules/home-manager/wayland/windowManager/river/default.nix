{ config, lib, pkgs, ... }:

/*
  TODO:
  - Client side decorations config
*/

with lib;

let
  cfg = config.wayland.windowManager.river;

  genKeybinding = mode:
    let
      binds = with cfg.config; zipLists (lib.attrNames keybindings.${mode}) (lib.attrValues keybindings.${mode});
    in
    toString (forEach binds (x: "riverctl map ${mode} ${x.fst} ${x.snd}\n"));

  configFile = pkgs.writeShellScriptBin "init" ''
    ${config.lib.shell.exportAll cfg.extraSessionVariables}

    # NORMAL KEYBINDINGS
    ${genKeybinding "normal"}

    # LOCKED KEYBINDINGS
    ${genKeybinding "locked"}

    riverctl declare-mode passthrough
    # PASSTHROUGH KEYBINDINGS
    ${genKeybinding "passthrough"}

    riverctl background-color 0x${cfg.config.backgroundColor}
    riverctl border-color-focused 0x${cfg.config.border.color.focused}
    riverctl border-color-unfocused 0x${cfg.config.border.color.unfocused}

    riverctl set-repeat ${cfg.config.repeatRate}

    riverctl default-layout ${cfg.config.layoutGenerator.name}
    exec ${cfg.config.layoutGenerator.name} ${cfg.config.layoutGenerator.arguments}

    ${cfg.extraConfig}
  '';
in
{
  options.wayland.windowManager.river = {
    enable = mkEnableOption "river wayland compositor";

    package = mkOption {
      type = with types; nullOr package;
      default = pkgs.river;
      defaultText = literalExpression "${pkgs.river}";
      description = "River package to use.";
    };

    xwayland = mkOption {
      type = types.bool;
      default = true;
      description = "Enable XWayland.";
    };

    extraSessionVariables = mkOption {
      type = types.attrs;
      default = { };
      description = "Extra session variables set when running the compositor.";
      example = { MOZ_ENABLE_WAYLAND = "1"; };
    };

    config = {
      backgroundColor = mkOption {
        type = types.string;
        default = "0x002b36";
        description = "Background color in rrggbb format.";
        example = "ffffff";
      };

      border = mkOption {
        type = types.submodule {
          options = {
            color = mkOption {
              type = (types.submodule {
                options = {
                  focused = mkOption {
                    type = types.string;
                    default = "93a1a1";
                    description = "Focused border color.";
                  };

                  unfocused = mkOption {
                    type = types.string;
                    default = "586e75";
                    description = "Unfocused border color.";
                  };
                };
              });
            };
          };
        };
      };

      layoutGenerator = mkOption {
        types = (types.submodule {
          options = {
            name = mkOption {
              type = types.string;
              default = "rivertile";
              description = "Name of the layout generator executable.";
            };

            arguments = mkOption {
              type = types.string;
              default = "-view-padding 6 -outer-padding 8";
              description = "Arguments passed to the layout generator.";
            };
          };
        });
      };

      repeatRate = mkOption {
        types = types.string;
        default = "50 300";
        description = "The repeat rate of the compositor.";
      };

      keybindings = mkOption {
        type = types.attrs;
        default = { };
        description = "An attribute set that assigns a key press to an action in a mode using a key symbol.";

        example = {
          normal = {
            "Alt Q" = "close";
            "Alt Return" = "spawn foot";
          };

          locked = {
            "None XF86AudioRaiseVolume" = "spawn 'pamixer -i 5'";
            "None XF86AudioRaiseVolume" = "spawn 'pamixer -d 5'";
          };

          passthrough = {
            "Alt F11" = "enter-mode normal";
          };
        };
      };
    };

    extraConfig = mkOption {
      type = types.string;
      default = "";
      description = "Extra lines appended to <filename>$XDG_CONFIG_HOME/river/init</filename>";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ ]
      ++ (optional (!cfg.xwayland) [ (pkgs.river.override { xwaylandSupport = false; }) ])
      ++ (optional cfg.xwayland [ pkgs.river pkgs.xwayland ]);

    xdg.configFile."river/init".source = configFile;
  };
}
