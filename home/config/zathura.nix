{ colors }:

{
  window-title-basename = true;
  selection-clipboard = "clipboard";

  notification-error-bg = "#${colors.c1}";
  notification-error-fg = "#${colors.bg}";
  notification-warning-bg = "#${colors.c3}";
  notification-warning-fg = "#${colors.bg}";
  notification-bg = "#${colors.c2}";
  notification-fg = "#${colors.bg}";

  completion-bg = "#${colors.bg}";
  completion-fg = "#${colors.fg}";
  completion-group-bg = "#${colors.bg}";
  completion-group-fg = "#${colors.fg}";
  completion-highlight-bg = "#${colors.c2}";
  completion-highlight-fg = "#${colors.fg}";

  index-bg = "#${colors.bg}";
  index-fg = "#${colors.fg}";
  index-active-bg = "#${colors.c2}";
  index-active-fg = "#${colors.bg}";

  inputbar-bg = "#${colors.bg}";
  inputbar-fg = "#${colors.fg}";
  statusbar-bg = "#${colors.bg}";
  statusbar-fg = "#${colors.fg}";

  highlight-color = "#${colors.c3}";
  highlight-active-color = "#${colors.c5}";

  default-bg = "#${colors.bg}";
  default-fg = "#${colors.fg}";

  render-loading = true;
  render-loading-fg = "#${colors.bg}";
  render-loading-bg = "#${colors.fg}";

  recolor-lightcolor = "#${colors.bg}";
  recolor-darkcolor = "#${colors.fg}";

  adjust-open = "width";
  recolor = true;
}
