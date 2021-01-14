{
  enable = true;
  refreshRate = 60;
  experimentalBackends = true;
  backend = "glx";
  vSync = true;
  settings = {
    blur = {
      method = "dual_kawase";
      strength = 5;
      background = false;
      background-frame = false;
      background-fixed = false;
    };
    blur-background-exclude = [
      "window_type = 'dock'"
      "window_type = 'desktop'"
      "_GTK_FRAME_EXTENTS@:c"
    ];
    use-ewmh-active-win = true;
    corner-radius = 8;
    round-borders = 1;
    rounded-corners-exclude = [
      "window_type = 'dock'"
      "window_type = 'desktop'"
      "!name ~= ''" # Exclude any "Unknown" windows
    ];
    transition-length = 200;
  };
}
