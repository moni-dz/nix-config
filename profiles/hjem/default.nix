{
  imports = [
    ./atuin.nix
    ./direnv.nix
    ./gh.nix
    ./git.nix
    ./htop.nix
    ./jujutsu.nix
    ./neovim.nix
    ./nushell.nix
    ./packages.nix
    ./rex.nix
    ./starship.nix
    ./typst.nix
    ./yazi.nix
    ./zoxide.nix
  ];

  environment.sessionVariables = {
    EDITOR = "nvim";
    DIRENV_LOG_FORMAT = "";
  };
}
