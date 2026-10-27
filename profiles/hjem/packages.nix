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
      msedit
      nixd
      nixfmt
      parallel
      ripgrep
      ;
  };
}
