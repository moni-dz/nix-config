{
  lib,
  pkgs,
  ...
}:

{
  rum.programs = {
    git = {
      enable = true;

      settings = {
        alias = {
          df = "difftool";
          a = "add";
          p = "push";
          r = "rebase";
          ri = "rebase -i";
          cm = "commit";
          pl = "pull";
          s = "status";
          st = "stash";
          ck = "checkout";
          rl = "reflog";
        };

        diff.tool = "difftastic";
        pager.difftool = true;

        difftool = {
          prompt = false;
          difftastic.cmd = ''${lib.getExe pkgs.difftastic} "$LOCAL" "$REMOTE"'';
        };
      };
    };
  };
}
