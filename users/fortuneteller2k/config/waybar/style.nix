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
    background: #${base00};
    color: #${base0D};
  }

  #workspaces button {
    padding: 0 6px;
    background: #${base0D};
    color: #${base01};
    border-bottom: 2px solid #${base0D};
  }

  #workspaces button.focused {
    background: #${base0D};
    border-bottom: 2px solid #${base01};
  }

  #clock,
  #pulseaudio {
    padding: 0 6px;
    margin: 0;
    background-color: #${base0D};
    color: #${base01};
  }

  #pulseaudio {
    background-color: #${base0E};
  }
''
