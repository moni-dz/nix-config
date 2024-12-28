{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.packages = __attrValues {
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

      settings = {
        core.fsmonitor = "watchman";
        format.tree-level-conflicts = true;

        aliases = {
          ds = [
            "desc"
            "-m"
          ];

          df = [ "diff" ];

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
          push-bookmark-prefix = "moni/";
        };

        signing = {
          sign-all = "true";
          backend = "ssh";
          key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBlr5SHXali3ttWt8ygyWgCW2usWVsBhXebeyi2XKO2Z";
        };

        ui = {
          editor = "nvim";

          default-command = [
            "log"
            "-r"
            "reachable(@, mutable())"
          ];

          diff.tool = [
            "${lib.getExe pkgs.difftastic}"
            "--color=always"
            "$left"
            "$right"
          ];

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
