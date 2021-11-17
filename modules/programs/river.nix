{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.programs.river;
in
{
  options.programs.river = {
    enable = mkEnableOption "river, a dynamic tiling Wayland compositor";

    extraPackages = mkOption {
      type = types.listOf types.package;
      default = with pkgs; [
        swaylock
        swayidle
        alacritty
        bemenu
        brightnessctl
        wdisplays
      ];
      defaultText = literalExample ''
        with pkgs; [ swaylock swayidle bemenu alacritty brightnessctl wdisplays ];
      '';
      example = literalExample ''
        with pkgs; [
          grim
          waybar
          rofi
        ]
      '';
      description = ''
        Extra packages to be installed system wide.
      '';
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.river ] ++ cfg.extraPackages;
    security.pam.services.swaylock = { };
    hardware.opengl.enable = mkDefault true;
    fonts.enableDefaultFonts = mkDefault true;
    programs.dconf.enable = mkDefault true;
    programs.xwayland.enable = mkDefault true;

    services.greetd = {
      enable = true;

      settings = {
        default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd river";

        initial_session = {
          command = "river";
          user = "fortuneteller2k";
        };
      };
    };
  };

  meta.maintainers = with lib.maintainers; [ fortuneteller2k ];
}
