{
  format = "$all ";
  username = {
    style_user = "bold red";
    format = "[$user]($style) ";
    disabled = false;
    show_always = true;
  };
  hostname = {
    ssh_only = false;
    format = "at [$hostname](bold blue) ";
    disabled = false;
  };
  directory.format = "in [$path]($style)[$read_only]($read_only_style) ";
  nix_shell.symbol = "🌨  ";
  nodejs.symbol = "⬢  ";
  package.symbol = "📦  ";	
}
