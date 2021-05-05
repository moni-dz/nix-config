{ pkgs }:

{
  diagnostic-languageserver.filetypes.sh = "${pkgs.shellcheck}/bin/shellcheck";

  languageserver.nix = {
    command = "${pkgs.rnix-lsp}/bin/rnix-lsp";
    filetypes = [ "nix" ];
  };
}
