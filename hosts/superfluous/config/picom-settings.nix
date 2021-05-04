{
  blur = {
    method = "dual_kawase";
    strength = 7;
    background = false;
    background-frame = false;
    background-fixed = false;
  };

  blur-background-exclude = [
    "window_type = 'dock'"
    "window_type = 'desktop'"
    "_GTK_FRAME_EXTENTS@:c"
  ];

  glx-no-stencil = true;
  glx-no-rebind-pixmap = true;
  unredir-if-possible = true;
  use-damage = true;
}
