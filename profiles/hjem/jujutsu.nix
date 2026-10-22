{
  config,
  lib,
  pkgs,
  ...
}:

{
  rum.programs = {
    jujutsu = {
      enable = true;
      integrations = {
        nushell.enable = true;
        starship.enable = true;
      };

      settings = {
        fsmonitor.backend = "watchman";

        aliases = {
          df = [ "diff" ];

          lm = [
            "log"
            "-r"
            "present(@) | present(ancestors(bookmarks() & mine() & mutable(), 5))"
          ];

          mv = [
            "b"
            "s"
            "-r"
          ];

          gp = [
            "git"
            "push"
          ];

          rba = [
            "rebase"
            "-b"
            "all:(mine() & mutable() & bookmarks())"
            "-d"
          ];
        };

        user = {
          name = "Lythe Marvin L. Lacre";
          email = "vcs@4rsxyzt.fyi";
        };

        git = {
          fetch = [
            "upstream"
            "origin"
          ];

          push = "origin";
        };

        signing = {
          behavior = "own";
          backend = "ssh";
          key = "${config.directory}/.ssh/id_ed25519.pub";
        };

        templates = {
          git_push_bookmark = ''"vcs/" ++ change_id.short()'';

          log = ''
                    if(root,
                      separate(" ",
                        format_short_change_id(self.change_id()),
                        format_short_commit_id(self.commit_id()),
                        label("root", "root()"),
                        self.bookmarks(),
                      ) ++ "
            ",
                      label(if(current_working_copy, "working_copy"),
                        concat(
                          separate(" ",
                            if(self.conflict(), label("conflict", "conflict")),
                            if(!empty, label("empty", "∆")),
                            if(empty && self.parents().len() > 1, label("empty", "git-merge")),
                            format_short_change_id_with_change_offset(self),
                            format_short_commit_id(self.commit_id()),
                            self.bookmarks(),
                            self.tags(),
                            self.working_copies(),
                            if(self.contained_in("first_parent(@)"), label("git_head", "git_head()")),
                            commit_timestamp(self).local().format("%Y-%m-%d"),
                            if(author.name() && author.email(), concat("by ", separate(" as ", author.email().local(), author.name()))),
                            if(author.name() && !author.email(), author.name()),
                            if(!author.name() && author.email(), author.email().local()),
                            if(!author.name() && !author.email(), email_placeholder),
                            if(config("ui.show-cryptographic-signatures").as_boolean(),
                              format_short_cryptographic_signature(self.signature())),
                          )
                          ++ "
            ",
                          if(description, description.first_line() ++ "
            "),
                        ),
                      )
                    ) ++ "
            "
          '';

          log_node = ''
            coalesce(
              if(!self, label("elided", "▬")),
              label(
                separate(" ",
                  if(current_working_copy, "working_copy"),
                  if(immutable, "immutable"),
                  if(conflict, "conflict"),
                ),
                coalesce(
                  if(current_working_copy, "►"),
                  if(root, "⌂"),
                  if(immutable, "■"),
                  if(conflict, "▼"),
                  "□",
                )
              )
            )
          '';

          op_log_node = ''
            coalesce(
              if(current_operation, label("current_operation", "►")),
              "□",
            )
          '';
        };

        ui = {
          editor = "nvim";
          graph.style = "square";
          show-cryptographic-signatures = false;

          default-command = [
            "log"
            "-r"
            "present(@) | ancestors(remote_bookmarks().., 2) | present(trunk()) | reachable(@, all())"
            "-n"
            "8"
          ];

          diff-formatter = [
            "${lib.getExe pkgs.difftastic}"
            "--color=always"
            "$left"
            "$right"
          ];

          paginate = "never";
        };
      };

    };
  };
}
