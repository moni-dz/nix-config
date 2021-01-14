{
  "sway/workspaces" = {
    all-outputs = true;
    persistent_workspaces = {
      "α" = [ ];
      "β" = [ ];
      "γ" = [ ];
      "δ" = [ ];
      "ε" = [ ];
      "ζ" = [ ];
      "η" = [ ];
      "θ" = [ ];
      "ι" = [ ];
      "κ" = [ ];
    };
  };
  "clock" = {
    interval = 10;
    format = "{:  %B %d %Y %I:%M %p}";
  };
  "battery" = {
    bat = "BAT0";
    states = {
      warning = 30;
      critical = 15;
    };
    format = "{icon} {capacity}%";
    format-charging = "{icon} {capacity}%";
    format-plugged = "{icon} {capacity}%";
    format-full = "{icon} {capacity}%";
    format-icons = [ "" "" "" "" "" ];
  };
  "network" = {
    interface = "wlo1";
    format = "{ifname}";
    format-wifi = "  {essid} ({signalStrength}%)";
    format-ethernet = "  {ifname}";
    format-disconnected = "  Disconnected";
    max-length = 50;
  };
  "pulseaudio" = {
    format = "{icon} {volume}%";
    format-muted = "婢  Muted";
    format-source = "";
    format-source-muted = "Muted";
    format-icons.default = [ " " " " " " ];
  };
}
