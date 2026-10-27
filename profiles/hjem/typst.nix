{
  pkgs,
  ...
}:

{
  packages = __attrValues {
    inherit (pkgs)
      tinymist
      typst
      ;
  };
}
