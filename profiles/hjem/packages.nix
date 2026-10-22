{
  pkgs,
  ...
}:

{
  packages = __attrValues {
    inherit (pkgs)
      curl
      difftastic
      fd
      gh
      gnupg
      jq
      krabby
      mosh
      msedit
      nixd
      nixfmt
      nix-output-monitor
      parallel
      ripgrep
      tinymist
      typst
      ;
  };
}
