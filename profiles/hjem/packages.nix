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
      gnupg
      jq
      nixd
      nixfmt
      ripgrep
      ;
  };
}
