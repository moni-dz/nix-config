{
  "colors" = {
    x0 = "\${xrdb:color0}";
    x1 = "\${xrdb:color1}";
    x2 = "\${xrdb:color2}";
    x3 = "\${xrdb:color3}";
    x4 = "\${xrdb:color4}";
    x5 = "\${xrdb:color5}";
    x6 = "\${xrdb:color6}";
    x7 = "\${xrdb:color7}";
  };

  "module/ewmh" = {
    type = "internal/xworkspaces";
    show-all = true;
    pin-workspaces = true;
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
    label-dimmed = "%index%";
    label-dimmed-padding = 1;
    label-active = "*";
    label-active-foreground = "\${colors.x0}";
    label-active-padding = 1;
    label-occupied = "%icon%";
    label-occupied-foreground = "\${colors.x0}";
    label-occupied-padding = 1;
    label-empty = "_";
    label-empty-foreground = "\${colors.x0}";
    label-empty-padding = 1;
    label-urgent = "!!";
    label-urgent-foreground = "\${colors.x0}";
    label-urgent-padding = 1;
    format = "   <label-state>";
    format-background = "\${colors.x5}";
    format-foreground = "\${colors.x0}";
  };

  "module/xwindow" = {
    type = "internal/xwindow";
    format = "<label>";
    format-foreground = "\${colors.x0}";
    format-background = "\${colors.x2}";
    format-padding = 1;
    label = "%title%";
    label-maxlen = 40;
  };

  "module/backlight" = {
    type = "internal/backlight";
    card = "radeon_bl0";
    enable-scroll = false;
    format = "<ramp> <label>";
    format-foreground = "\${colors.x0}";
    format-background = "\${colors.x5}";
    label = "%percentage%% ";
    ramp-0 = "  ";
    ramp-1 = "  ";
    ramp-2 = "  ";
    ramp-3 = "  ";
  };

  "module/battery" = {
    type = "internal/battery";
    full-at = 99;
    battery = "BAT0";
    adapter = "ACAD";
    format-charging = "<ramp-capacity> <label-charging>";
    format-charging-foreground = "\${colors.x0}";
    format-charging-background = "\${colors.x5}";
    format-discharging = "<ramp-capacity> <label-discharging>";
    format-discharging-foreground = "\${colors.x0}";
    format-discharging-background = "\${colors.x5}";
    format-full = "<label-full>";
    format-full-foreground = "\${colors.x0}";
    format-full-background = "\${colors.x5}";
    label-charging = "%percentage%% ";
    label-discharging = "%percentage%% ";
    label-full = "  %percentage%% ";
    ramp-capacity-0 = " ";
    ramp-capacity-1 = " ";
    ramp-capacity-2 = " ";
    ramp-capacity-3 = " ";
    ramp-capacity-4 = " ";
    ramp-capacity-5 = " ";
    ramp-capacity-6 = " ";
    ramp-capacity-7 = " ";
    ramp-capacity-8 = " ";
    ramp-capacity-9 = " ";
  };

  "module/pulseaudio" = {
    type = "internal/pulseaudio";
    sink = "alsa_output.pci-0000_00_14.2.analog-stereo";
    use-ui-max = true;
    interval = 10;
    format-volume = " <ramp-volume> <label-volume> ";
    format-volume-foreground = "\${colors.x0}";
    format-volume-background = "\${colors.x5}";
    label-muted = " 婢  Muted ";
    label-muted-foreground = "\${colors.x0}";
    label-muted-background = "\${colors.x5}";
    ramp-volume-0 = "  ";
    ramp-volume-1 = "  ";
    ramp-volume-2 = "  ";
  };

  "module/network" = {
    type = "custom/script";
    exec = "\"PATH=/run/current-system/sw/bin /home/fortuneteller2k/.config/scripts/network.sh\"";
    interval = 10;
    tail = true;
    click-left = "\"PATH=/home/fortuneteller2k/.nix-profile/bin networkmanager_dmenu &\"";
    format = "<label>";
    format-foreground = "\${colors.x0}";
    format-background = "\${colors.x5}";
    label = "%output% ";
  };

  "module/date" = {
    type = "internal/date";
    interval = 1;
    date = "   %B %d %Y";
    time = "%I:%M %p";
    format = "<label>";
    format-foreground = "\${colors.x0}";
    format-background = "\${colors.x5}";
    label = "%date% %time%";
  };

  "module/wspc" = {
    type = "custom/text";
    content = " ";
    content-foreground = "\${colors.x5}";
    content-background = "\${colors.x5}";
  };

  "module/wspc_b" = {
    type = "custom/text";
    content = " ";
    content-foreground = "#00000000";
    content-background = "#00000000";
  };

  "bar/main" = {
    override-redirect = true;
    pseudo-transparency = true;
    fixed-center = true;
    background = "#00000000";
    foreground = "\${colors.x7}";
    width = "100%";
    height = 19;
    wm-name = "spectrwm";
    enable-ipc = true;
    font-0 = "FantasqueSansMono Nerd Font:size=12:style=Medium;3";
    modules-left = "wspc ewmh";
    modules-right = "backlight wspc_b wspc battery wspc_b pulseaudio wspc_b wspc network wspc_b date wspc";
    modules-center = "";
    locale = "en_US.UTF-8";
    module-margin-left = 0;
    module-margin-right = 0;
    padding = 1;
    separator = "";
  };
}
