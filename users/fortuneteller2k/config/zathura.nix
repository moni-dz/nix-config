{ theme }:

with theme.colors; {
  window-title-basename = true;
  selection-clipboard = "clipboard";

  notification-error-bg = "#${c1}";
  notification-error-fg = "#${bg}";
  notification-warning-bg = "#${c3}";
  notification-warning-fg = "#${bg}";
  notification-bg = "#${c2}";
  notification-fg = "#${bg}";

  completion-bg = "#${bg}";
  completion-fg = "#${fg}";
  completion-group-bg = "#${bg}";
  completion-group-fg = "#${fg}";
  completion-highlight-bg = "#${c2}";
  completion-highlight-fg = "#${bg}";

  index-bg = "#${bg}";
  index-fg = "#${fg}";
  index-active-bg = "#${c2}";
  index-active-fg = "#${bg}";

  inputbar-bg = "#${bg}";
  inputbar-fg = "#${fg}";
  statusbar-bg = "#${bg}";
  statusbar-fg = "#${fg}";

  highlight-color = "#${c3}";
  highlight-active-color = "#${c5}";

  default-bg = "#${bg}";
  default-fg = "#${fg}";

  render-loading = true;
  render-loading-fg = "#${bg}";
  render-loading-bg = "#${fg}";

  recolor-lightcolor = "#${bg}";
  recolor-darkcolor = "#${fg}";

  adjust-open = "width";
  recolor = true;
}
