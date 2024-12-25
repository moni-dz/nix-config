{
  config,
  lib,
  pkgs,
  self',
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
      inherit (config.programs.git) enable;
      #package = self'.packages.jujutsu.overrideAttrs { doCheck = false; };

      settings = {
        core.fsmonitor = "watchman";
        format.tree-level-conflicts = true;

        aliases = {
          l = [
            "log"
            "-r"
            "reachable(@, mutable())"
          ];

          lm = [
            "log"
            "-r"
            "(master..@):: | (master..@)-"
          ];

          lmain = [
            "log"
            "-r"
            "(main..@):: | (main..@)-"
          ];

          mv = [
            "bookmark"
            "set"
            "--revision"
          ];

          gp = [
            "git"
            "push"
          ];
        };

        user = {
          name = "moni-dz";
          email = "lythe1107@gmail.com";
        };

        git = {
          fetch = [
            "upstream"
            "origin"
          ];

          push = "origin";
        };

        ui = {
          editor = "nvim";

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
