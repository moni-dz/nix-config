{ theme, pkgs }:

with theme; {
  "colors" = {
    x0 = "#${if lightModeEnabled then colors.c7 else colors.c0}";
    x1 = "#${colors.c1}";
    x2 = "#${colors.c2}";
    x3 = "#${colors.c3}";
    x4 = "#${colors.c4}";
    x5 = "#${colors.c5}";
    x6 = "#${colors.c6}";
    x7 = "#${if lightModeEnabled then colors.c0 else colors.c7}";
    bg = "#${colors.transparent}";
  };

  "module/ewmh" = {
    type = "internal/xworkspaces";
    show-all = true;
    pin-workspaces = false;
    enable-click = true;
    enable-scroll = true;
    icon-0 = "A;α";
    icon-1 = "B;β";
    icon-2 = "C;γ";
    icon-3 = "D;δ";
    icon-4 = "E;ε";
    icon-5 = "F;ζ";
    icon-6 = "G;η";
    icon-7 = "H;θ";
    icon-8 = "I;ι";
    icon-9 = "J;κ";
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
    format-background = "#${colors.primary}";
    format-foreground = "#${colors.textColor}";
  };

  "module/mpd" = {
    type = "internal/mpd";
    host = "127.0.0.1";
    port = "6600";
    format-online = "%{A1:playerctl --player=mpd play-pause:}<label-song>%{A}";
    format-online-background = "#${colors.primary}";
    format-online-foreground = "#${colors.textColor}";
    label-song = " ﱘ  %title% ";
    label-song-maxlen = 40;
  };

  "module/xmonad" = {
    type = "custom/script";
    exec = "${pkgs.xmonad-log}/bin/xmonad-log";
    tail = true;
    label = ": %output% ";
    format = "<label>";
    format-background = "#${colors.primary}";
    format-foreground = "${colors.textColor}";
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
    format-volume-background = "#${colors.primary}";
    label-muted = " 婢  Muted ";
    label-muted-foreground = "#${colors.textColor}";
    label-muted-background = "#${colors.primary}";
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
    format-background = "#${colors.primary}";
    label = "%date% %time% ";
  };

  "module/wspc" = {
    type = "custom/text";
    content = " ";
    content-foreground = "#${colors.primary}";
    content-background = "#${colors.primary}";
  };

  "module/wspc_b" = {
    type = "custom/text";
    content = " ";
    content-foreground = "\${colors.bg}";
    content-background = "\${colors.bg}";
  };

  "settings" = {
    pseudo-transparency = true;
  };

  "bar/main" = {
    override-redirect = true;
    fixed-center = true;
    background = "\${colors.bg}";
    foreground = "#${colors.fg}";
    width = "100%";
    height = 17;
    wm-name = "polybar-xmonad";
    enable-ipc = true;
    font-0 = "FantasqueSansMono Nerd Font:size=10.5;2";
    modules-left = "wspc ewmh xmonad";
    modules-center = "mpd";
    modules-right = "battery wspc_b pulseaudio wspc_b wireless wspc_b date";
    locale = "en_US.UTF-8";
    border-size = 0;
    separator = "";
  };
}
