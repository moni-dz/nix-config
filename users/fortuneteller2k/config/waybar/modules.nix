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
    format = "{:  %m/%d/%Y %I:%M %p}";
  };

  "network" = {
    interface = "wlan0";
    format = "  {ifname}";
    format-wifi = "  {essid}";
    format-ethernet = "  {essid}";
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
