{ colorscheme, iconsDirectory }:

with colorscheme.colors;

''
    * {
      background-image: none;
    }

    window {
      background-color: #${base00};
    }

    button {
      color: #${base05};
      background-color: #${base02};
      border: 4px solid #${base00};
      border-style: double;
      background-repeat: no-repeat;
      background-position: center;
      background-size: 15%;
    }

    button:focus, button:active, button:hover {
      border: 4px solid #${base0D};
      outline-style: none;
    }

  #lock {
    background-image: image(url("${iconsDirectory}/lock.png"), url("${iconsDirectory}/lock.png"));
  }

  #logout {
    background-image: image(url("${iconsDirectory}/logout.png"), url("${iconsDirectory}/logout.png"));
  }

  #shutdown {
    background-image: image(url("${iconsDirectory}/shutdown.png"), url("${iconsDirectory}/shutdown.png"));
  }

  #reboot {
    background-image: image(url("${iconsDirectory}/reboot.png"), url("${iconsDirectory}/reboot.png"));
  }
''
