{ config, lib, pkgs, ... }:

/*
  TODO:
  - Client side decorations config
*/

with lib;

let
  cfg = config.wayland.windowManager.river;

  genKeybind = mode:
    let
      binds = with cfg.config; zipLists (attrNames keybindings.${mode}) (attrValues keybindings.${mode});
    in
    toString (forEach binds (x: "riverctl map ${mode} ${x.fst} ${x.snd}\n"));

  genMousebind =
    let
      binds = with cfg.config; zipLists (attrNames keybindings.pointer) (attrValues keybindings.pointer);
    in
    toString (forEach binds (x: "riverctl map-pointer normal ${x.fst} ${x.snd}\n"));

  configFile = pkgs.writeShellScript "init" ''
    ### This file was generated with Nix. Don't modify this file directly. 

    ${config.lib.shell.exportAll cfg.extraSessionVariables}
    
    riverctl declare-mode passthrough

    # PASSTHROUGH KEYBINDINGS
    ${genKeybind "passthrough"}
    
    # NORMAL KEYBINDINGS
    ${genKeybind "normal"}

    # MOUSE BINDINGS
    ${genMousebind}

    # LOCKED KEYBINDINGS
    ${genKeybind "locked"}

    riverctl declare-mode passthrough
    # PASSTHROUGH KEYBINDINGS
    ${genKeybind "passthrough"}

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
      type = types.package;
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
        type = types.str;
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
                    type = types.str;
                    default = "93a1a1";
                    description = "Focused border color.";
                  };

                  unfocused = mkOption {
                    type = types.str;
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
        type = (types.submodule {
          options = {
            name = mkOption {
              type = types.str;
              default = "rivertile";
              description = "Name of the layout generator executable.";
            };

            arguments = mkOption {
              type = types.str;
              default = "-view-padding 6 -outer-padding 8";
              description = "Arguments passed to the layout generator.";
            };
          };
        });
      };

      repeatRate = mkOption {
        type = types.str;
        default = "50 300";
        description = "The repeat rate of the compositor.";
      };

      keybindings = mkOption {
        type = types.attrs;
        
        default = {
          normal = { };
          locked = { };
          passthrough = { };
          pointer = { };
        };

        description = "An attribute set that assigns a key press to an action in a mode using a key symbol.";

        example = lib.mkOptionDefault {
          normal = {
            "Alt Q" = "close";
            "Alt Return" = "spawn foot";
          };

          locked = {
            "None XF86AudioRaiseVolume" = "spawn 'pamixer -i 5'";
            "None XF86AudioLowerVolume" = "spawn 'pamixer -d 5'";
          };

          passthrough = {
            "Alt F11" = "enter-mode normal";
          };

          pointer = {
            "Alt BTN_LEFT" = "move-view"; 
          };
        };
      };
    };

    extraConfig = mkOption {
      type = types.lines;
      default = "";
      description = "Extra lines appended to <filename>$XDG_CONFIG_HOME/river/init</filename>";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ (cfg.package.override { xwaylandSupport = cfg.xwayland; }) ]
      ++ (optional cfg.xwayland pkgs.xwayland);

    xdg.configFile."river/init".source = configFile;
  };
}
