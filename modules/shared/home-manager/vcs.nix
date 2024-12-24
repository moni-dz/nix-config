{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.packages = __attrValues {
    inherit (pkgs) difftastic;
    inherit (pkgs.gitAndTools) gh;
  };

  programs = {
    git = {
      enable = true;

      aliases = {
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

      extraConfig = {
        diff.tool = "difftastic";
        pager.difftool = true;

        difftool = {
          prompt = false;
          difftastic.cmd = ''${lib.getExe pkgs.difftastic} "$LOCAL" "$REMOTE"'';
        };
      };
    };

    jujutsu = {
      enable = config.programs.git.enable;
      settings = {
        user = {
          name = "moni-dz";
          email = "lythe1107@gmail.com";
        };

        ui = {
          diff-editor = [
            "nvim"
            "-c"
            "DiffEditor $left $right $output"
          ];

          paginate = "never";
        };
      };
    };
  };
}
