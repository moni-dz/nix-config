''
  * {
    border: none;
    border-radius: 0;
    font-family: monospace;
    font-size: 14px;
    min-height: 0;
  }

  window#waybar {
    background: #16161c;
    color: #fdf0ed;
  }

  #workspaces button {
    padding: 0 4px;
    background: transparent;
    color: #fdf0ed;
    border-bottom: 2px solid transparent;
  }

  #workspaces button.focused {
    background: #232530;
    border-bottom: 2px solid #e95678;
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
    padding: 0 8px;
    margin: 0;
  }

  #clock {
    background-color: #16161c;
  }

  #network {
    background-color: #16161c;
  }

  @keyframes blink {
    to {
      background-color: #26a65b;
      color: #fb4934;
    }
  }

  #battery {
    background-color: #98971a;
    color: #1d2021;
  }

  #battery.full {
    background-color: transparent;
    color: #29d398;
  }

  #battery.discharging {
    background-color: transparent;
    color: #fab795;
  }

  #battery.charging {
    background-color: transparent;
    color: #3fdaa4;
  }

  #battery.warning:not(.charging) {
    background: transparent;
    color: #f6661e;
    animation-name: blink;
    animation-duration: 0.5s;
    animation-timing-function: linear;
    animation-iteration-count: infinite;
    animation-direction: alternate;
  }
''
