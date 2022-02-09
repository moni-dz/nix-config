{
  "sway/workspaces" = {
    all-outputs = true;
    disable-scroll = true;
    format = "{icon}";

    format-icons = {
      "1" = "α";
      "2" = "β";
      "3" = "γ";
      "4" = "δ";
      "5" = "ε";
      "6" = "ζ";
      "7" = "η";
      "8" = "θ";
      "9" = "ι";
      "10" = "κ";
    };

    persistent_workspaces = {
      "1" = [ ];
      "2" = [ ];
      "3" = [ ];
      "4" = [ ];
      "5" = [ ];
      "6" = [ ];
      "7" = [ ];
      "8" = [ ];
      "9" = [ ];
      "10" = [ ];
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
