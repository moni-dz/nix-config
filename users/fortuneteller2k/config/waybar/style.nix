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
    background: #${bg};
    color: #${fg};
  }

  #workspaces button {
    padding: 0 6px;
    background: #${primaryBright};
    color: #${bg};
    border-bottom: 2px solid #${primaryBright};
  }

  #workspaces button.focused {
    background: #${primaryBright};
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
    background-color: #${primaryBright};
    color: #${bg};
  }

  @keyframes blink {
    to {
      background-color: #${primaryBright};
      color: #${bg};
    }
  }

  #network {
    background-color: #${secondaryBright};
  }

  #battery {
    background-color: #${secondaryBright};
    color: #${bg};
  }

  #battery.full {
    background-color: #${secondaryBright};
    color: #${bg};
  }

  #battery.discharging {
    background-color: #${secondaryBright};
    color: #${bg};
  }

  #battery.charging {
    background-color: #${secondaryBright};
    color: #${bg};
  }

  #battery.warning:not(.charging) {
    background-color: #${secondaryBright};
    color: #${bg};
    animation-name: blink;
    animation-duration: 0.5s;
    animation-timing-function: linear;
    animation-iteration-count: infinite;
    animation-direction: alternate;
  }
''
