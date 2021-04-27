{ theme }:

with theme.colors;

''
  * {
    border: none;
    border-radius: 0;
    font-family: "FantasqueSansMono Nerd Font";
    font-size: 14px;
    min-height: 0;
  }

  window#waybar {
    background: transparent;
    color: #${fg};
  }

  #workspaces button {
    padding: 0 6px;
    background: #${primary};
    color: #${bg};
    border-bottom: 2px solid #${primary};
  }

  #workspaces button.focused {
    background: #${primary};
    border-bottom: 2px solid #${bg};
  }

  #clock,
  #battery,
  #cpu,
  #memory,
  #temperature,
  #backlight,
  #network,
  #pulseaudio,
  #tray,
  #mode,
  #mpd {
    padding: 0 6px;
    margin: 0;
    background-color: #${primary};
    color: #${bg};
  }

  @keyframes blink {
    to {
      background-color: #${primary};
      color: #${bg};
    }
  }

  #battery {
    background-color: #${primary};
    color: #${bg};
  }

  #battery.full {
    background-color: #${primary};
    color: #${bg};
  }

  #battery.discharging {
    background-color: #${primary};
    color: #${bg};
  }

  #battery.charging {
    background-color: #${primary};
    color: #${bg};
  }

  #battery.warning:not(.charging) {
    background-color: #${primary};
    color: #${bg};
    animation-name: blink;
    animation-duration: 0.5s;
    animation-timing-function: linear;
    animation-iteration-count: infinite;
    animation-direction: alternate;
  }
''
