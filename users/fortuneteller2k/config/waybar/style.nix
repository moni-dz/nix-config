{ colorscheme }:

with colorscheme.colors;

''
  * {
    border: none;
    border-radius: 0;
    font-family: "FantasqueSansMono Nerd Font";
    font-size: 14px;
    min-height: 0;
  }

  window#waybar {
    background: #${base01};
    color: #${base05};
  }

  #workspaces button {
    padding: 0 6px;
    background: #${base0B};
    color: #${base01};
    border-bottom: 2px solid #${base0B};
  }

  #workspaces button.focused {
    background: #${base0B};
    border-bottom: 2px solid #${base01};
  }

  #clock,
  #network,
  #pulseaudio {
    padding: 0 6px;
    margin: 0;
    background-color: #${base0B};
    color: #${base01};
  }

  #network {
    background-color: #${base08};
  }
''
