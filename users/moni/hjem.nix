{
  config,
  lib,
  pkgs,
  osConfig,
  self',
  infuse,
  ...
}:

{
  environment.sessionVariables = {
    EDITOR = "edit";
    DIRENV_LOG_FORMAT = "";
  };

  packages = __attrValues {
    inherit (pkgs)
      curl
      difftastic
      eternal-terminal
      fd
      gh
      gnupg
      iproute2mac
      jq
      krabby
      mosh
      msedit
      nixd
      nixfmt
      nix-output-monitor
      parallel
      ripgrep
      tinymist
      typst
      watchman
      ;
  };

  rum.programs = {
    atuin = {
      enable = true;
      integrations.nushell.enable = true;

      settings = {
        style = "full";
        sync_frequency = "5m";
      };
    };

    direnv = {
      enable = true;
      integrations.nushell.enable = true;
    };

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

    htop = {
      enable = true;

      settings = {
        detailed_cpu_time = true;
        hide_kernel_threads = false;
        show_cpu_frequency = pkgs.stdenv.isLinux;
        show_cpu_usage = true;
        show_program_path = false;
        show_thread_names = true;

        # PID USER PRIORITY NICE M_SIZE M_RESIDENT M_SHARE STATE PERCENT_CPU PERCENT_MEM TIME COMM
        fields = [
          0
          48
          17
          18
          39
          40
          41
          2
          46
          47
          49
          1
        ];
        left_meters = [
          "AllCPUs"
          "Memory"
          "Swap"
        ];
        left_meter_modes = [
          1
          1
          1
        ];
        right_meters = [
          "Tasks"
          "LoadAverage"
          "Uptime"
        ];
        right_meter_modes = [
          2
          2
          2
        ];
      };
    };

    jujutsu = {
      enable = true;
      integrations.nushell.enable = true;

      package = infuse self'.packages.jujutsu {
        __output = {
          version.__prepend = "0.29.0-";
          doCheck.__assign = false;
        };
      };

      settings = {
        fsmonitor.backend = "watchman";
        format.tree-level-conflicts = true;

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

        signing = {
          behavior = "own";
          backend = "ssh";
          key = "${config.directory}/.ssh/id_ed25519.pub";
        };

        templates = {
          git_push_bookmark = ''"moni/" ++ change_id.short()'';

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
                            format_short_change_id_with_hidden_and_divergent_info(self),
                            format_short_commit_id(self.commit_id()),
                            self.bookmarks(),
                            self.tags(),
                            self.working_copies(),
                            if(self.git_head(), label("git_head", "git_head()")),
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
          editor = "edit";
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
    nushell = {
      enable = true;
      aliases = {
        vi = "edit";
        vim = "edit";
      };

      settings = {
        show_banner = false;
        buffer_editor = "edit";
      };

      envFile = ''
        open ${osConfig.age.secrets.tokens.path} | split column " " | get column2 | split column "=" | reduce -f {} {|it, acc| $acc | upsert $it.column1 $it.column2 } | load-env
        $env.BUN_INSTALL = ($env.HOME | path join ".bun")
        $env.path ++= ["/etc/profiles/per-user/${config.user}/bin" "/run/current-system/sw/bin" "/usr/bin" "/usr/local/bin" "/opt/homebrew/bin" "${config.directory}/.modular/bin" "/opt/local/bin" "/opt/homebrew/opt/llvm/bin" "${config.directory}/.local/share/modular/pkg/packages.modular.com_mojo/bin" ($env.BUN_INSTALL | path join "bin") "/Users/Shared/DBngin/postgresql/17.0/bin" "${config.directory}/.local/bin" "/nix/var/nix/profiles/default/bin"]
      '';
    };

    starship = {
      enable = true;
      integrations.nushell.enable = true;

      settings = {
        command_timeout = 3000;
        format = "$hostname$username$nix_shell$character";
        right_format = "$directory\${custom.jjid}\${custom.jjbookmark}\${custom.jjstat}$git_state$git_commit$git_status";

        character = {
          success_symbol = "[ ♥ ](fg:black bg:cyan)";
          error_symbol = "[ ♥ ](fg:black bg:red)";
          vimcmd_symbol = "[ ♡ ](fg:black bg:purple)";
          vimcmd_replace_symbol = "[ ♡ ](fg:black bg:green)";
          vimcmd_replace_one_symbol = "[ ♡ ](fg:black bg:green)";
          vimcmd_visual_symbol = "[ ♡ ](fg:black bg:yellow)";
        };

        username = {
          style_user = "bg:purple fg:black";
          style_root = "bg:red fg:black";
          format = "[ $user ]($style)";
          disabled = false;
          show_always = true;
        };

        hostname = {
          style = "fg:black bg:blue";
          ssh_only = true;
          ssh_symbol = "";
          format = "[ ✦ $hostname ✦ ]($style)";
          disabled = false;
        };

        git_commit = {
          style = "fg:black bg:purple";
          format = "[ $hash$tag ]($style)";
        };

        git_state = {
          style = "fg:black bg:red";
          format = "[ $state $progress_current/$progress_total ]($style)";
        };

        git_status = {
          style = "fg:black bg:red";
          ahead = "▲";
          behind = "▼";
          conflicted = "±";
          deleted = "×";
          diverged = "◊";
          up_to_date = "√";
          modified = "‼";
          staged = "+";
          renamed = "≡";
          stashed = "▽";
          untracked = "?";
          format = "[( $all_status$ahead_behind )]($style)";
        };

        git_branch = {
          style = "fg:black bg:green";
          format = "[ $symbol$branch(:$remote_branch) ]($style)";
          symbol = "";
        };

        battery.disabled = true;
        line_break.disabled = true;

        directory = {
          truncation_length = 2;
          style = "bg:blue fg:black";
          read_only_style = "bg:red fg:black";
          read_only = " RO ";
          format = "[$read_only]($read_only_style)[ $path ]($style)";
        };

        nix_shell = {
          style = "fg:black bg:yellow";
          format = "[ $name ]($style)";
        };

        custom =
          let
            jj-log = "jj log -r@ -n1 --no-graph --ignore-working-copy --color never -T";
          in
          {
            jjid = {
              ignore_timeout = true;
              description = "current jj revision";
              when = "jj root";
              command = "${jj-log} 'change_id.shortest(4)'";
              style = "fg:black bg:green";
              format = "[ $output ]($style)";
            };

            jjbookmark = {
              ignore_timeout = true;
              description = "jj bookmark of current revision";
              when = "jj root";
              command = "${jj-log} 'self.bookmarks()' | tr ' ' '
  ' | sort -k1.1n | head -1";
              style = "fg:black bg:purple";
              format = "[( $output )]($style)";
            };

            jjstat = {
              ignore_timeout = true;
              description = "current jj bookmark status";
              when = "jj root";
              style = "fg:black bg:red";
              format = "[( $output )]($style)";
              command = ''
                ${jj-log} '
                  separate("",
                    if(!empty, "∆"),
                    if(conflict, "${config.rum.programs.starship.settings.git_status.conflicted}"),
                    if(divergent, "${config.rum.programs.starship.settings.git_status.diverged}"),
                    if(hidden, "${config.rum.programs.starship.settings.git_status.deleted}"),
                    if(immutable, "λ"),
                    if(git_head, "★"),
                    if(root, "☆")
                  )
                '
              '';
            };
          };
      };
    };

    zoxide = {
      enable = true;
      integrations.nushell.enable = true;
    };
  };
}
