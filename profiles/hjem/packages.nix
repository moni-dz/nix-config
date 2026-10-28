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
      fennel-ls
      fnlfmt
      gnupg
      jq
      nixd
      nixfmt
      ripgrep
      ;
    inherit (pkgs.luaPackages) fennel;
  };
}
