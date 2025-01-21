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
            "present(@) | present(ancestors(bookmarks() & mine() & mutable(), 5))"
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

          ft = [
            "git"
            "fetch"
            "-b"
          ];

          rb = [
            "rebase"
            "-d"
          ];

          rba = [
            "rebase"
            "-b"
            "all:(mine() & mutable() & bookmarks())"
            "-d"
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
          sign-all = true;
          backend = "ssh";
          key = "${config.home.homeDirectory}/.ssh/id_ed25519.pub";
        };

        merge-tools.diffconflicts = {
          program = "nvim";
          merge-tool-edits-conflict-markers = true;

          merge-args = [
            "-c"
            "let g:jj_diffconflicts_marker_length=$marker_length"
            "-c"
            "JJDiffConflicts!"
            "$output"
            "$base"
            "$left"
            "$right"
          ];
        };

        ui = {
          editor = "nvim";

          default-command = [
            "log"
            "-r"
            "present(@) | ancestors(remote_bookmarks().., 2) | present(trunk()) | reachable(@, mutable() | ~mutable())"
            "-n"
            "8"
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
