{ theme, pkgs }:

with theme; {
  "module/ewmh" = {
    type = "internal/xworkspaces";
    show-all = true;
    pin-workspaces = false;
    enable-click = true;
    enable-scroll = true;
    icon-0 = "1;α";
    icon-1 = "2;β";
    icon-2 = "3;γ";
    icon-3 = "4;δ";
    icon-4 = "5;ε";
    icon-5 = "6;ζ";
    icon-6 = "7;η";
    icon-7 = "8;θ";
    icon-8 = "9;ι";
    icon-9 = "0;κ";
    label-dimmed = "%index%";
    label-dimmed-padding = 1;
    label-active = "*";
    label-active-foreground = "#${colors.textColor}";
    label-active-padding = 1;
    label-occupied = "%icon%";
    label-occupied-foreground = "#${colors.textColor}";
    label-occupied-padding = 1;
    label-empty = "_";
    label-empty-foreground = "#${colors.textColor}";
    label-empty-padding = 1;
    label-urgent = "!";
    label-urgent-foreground = "#${colors.textColor}";
    label-urgent-padding = 1;
    format = ''"    <label-state>"'';
    format-background = "#${colors.primaryBright}";
    format-foreground = "#${colors.textColor}";
  };

  "module/xmonad" = {
    type = "custom/script";
    exec = "${pkgs.xmonad-log}/bin/xmonad-log";
    tail = true;
    label = ": %output% ";
    format = "<label>";
    format-background = "#${colors.primaryBright}";
    format-foreground = "#${colors.textColor}";
  };

  "module/battery" = {
    type = "internal/battery";
    full-at = 99;
    battery = "BAT0";
    adapter = "ACAD";
    format-charging = "<ramp-capacity> <label-charging>";
    format-charging-foreground = "#${colors.textColor}";
    format-charging-background = "#${colors.primary}";
    format-discharging = "<ramp-capacity> <label-discharging>";
    format-discharging-foreground = "#${colors.textColor}";
    format-discharging-background = "#${colors.primary}";
    format-full = "<label-full>";
    format-full-foreground = "#${colors.textColor}";
    format-full-background = "#${colors.primary}";
    label-charging = "%percentage%% ";
    label-discharging = "%percentage%% ";
    label-full = "   %percentage%% ";
    ramp-capacity-0 = "  ";
    ramp-capacity-1 = "  ";
    ramp-capacity-2 = "  ";
    ramp-capacity-3 = "  ";
    ramp-capacity-4 = "  ";
    ramp-capacity-5 = "  ";
    ramp-capacity-6 = "  ";
    ramp-capacity-7 = "  ";
    ramp-capacity-8 = "  ";
    ramp-capacity-9 = "  ";
  };

  "module/pulseaudio" = {
    type = "internal/pulseaudio";
    sink = "alsa_output.pci-0000_00_14.2.analog-stereo";
    use-ui-max = true;
    interval = 10;
    format-volume = " <ramp-volume> <label-volume> ";
    format-volume-foreground = "#${colors.textColor}";
    format-volume-background = "#${colors.primaryBright}";
    label-muted = " 婢  Muted ";
    label-muted-foreground = "#${colors.textColor}";
    label-muted-background = "#${colors.primaryBright}";
    ramp-volume-0 = "  ";
    ramp-volume-1 = "  ";
    ramp-volume-2 = "  ";
  };

  "module/wireless" = {
    type = "internal/network";
    interface = "wlo1";
    interval = 10;
    format-connected = "<label-connected>";
    format-connected-foreground = "#${colors.textColor}";
    format-connected-background = "#${colors.primary}";
    format-disconnected = "<label-disconnected>";
    format-disconnected-foreground = "#${colors.textColor}";
    format-disconnected-background = "#${colors.primary}";
    label-connected = "   %essid% ";
    label-disconnected = "   Disconnected ";
  };

  "module/date" = {
    type = "internal/date";
    interval = 1;
    date = "   %m/%d/%Y";
    time = "%I:%M %p";
    format = "<label>";
    format-foreground = "#${colors.textColor}";
    format-background = "#${colors.primaryBright}";
    label = "%date% %time% ";
  };

  "module/window" = {
    type = "internal/xwindow";
    format = "<label>";
    format-foreground = "#${colors.muted}";
    format-background = "#${colors.transparent}";
    label = "%title%";
    label-maxlen = 50;
  };

  "module/wspc" = {
    type = "custom/text";
    content = " ";
    content-foreground = "#${colors.primaryBright}";
    content-background = "#${colors.primaryBright}";
  };

  "module/wspc_b" = {
    type = "custom/text";
    content = " ";
    content-foreground = "${colors.transparent}";
    content-background = "${colors.transparent}";
  };

  "bar/main" = {
    override-redirect = true;
    fixed-center = true;
    background = "#${colors.transparent}";
    foreground = "#${colors.fg}";
    width = "100%";
    height = 17;
    wm-name = "polybar-xmonad";
    enable-ipc = true;
    font-0 = "FantasqueSansMono Nerd Font:size=10.5;2";
    modules-left = "wspc ewmh xmonad";
    modules-center = "window";
    modules-right = "battery pulseaudio wireless date";
    locale = "en_US.UTF-8";
    border-size = 0;
    separator = "";
  };
}
