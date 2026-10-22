{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [ ./jujutsu.nix ];

  environment.sessionVariables = {
    EDITOR = "nvim";
    DIRENV_LOG_FORMAT = "";
  };

  packages = __attrValues {
    inherit (pkgs)
      curl
      difftastic
      fd
      gh
      gnupg
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
        show_cpu_frequency = pkgs.stdenv.hostPlatform.isLinux;
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

    neovim.enable = true;

    nushell = {
      enable = true;
      aliases = {
        vi = "nvim";
        vim = "nvim";
      };

      settings = {
        show_banner = false;
        buffer_editor = "nvim";
        render_right_prompt_on_last_line = true;
      };

      extraConfig = ''
        def rex-tab [] {
          let before = (commandline | str substring 0..<(commandline get-cursor))
          let word = ($before | split row -r '\s+' | last | default "")
          if ($word =~ '^(~|\.{1,2}$|.*/)') {
            commandline edit --replace $before  # keep the typed line; insert path goes after the cursor
            rex do pane.insert_path
          } else {
            stty -echo  # keep the ctrl+x below from being echoed; reedline restores the tty at the next prompt
            rex send-key -s $env.REX_SESSION -b $env.REX_BLOCK ctrl+x
          }
        }

        if ($env.REX_SESSION? | is-not-empty) { $env.config.keybindings ++= [
          {
            name: rex_tab
            modifier: none
            keycode: tab
            mode: [emacs vi_normal vi_insert helix_normal helix_select helix_insert]
            event: { until: [
              { send: menunext }
              { send: executehostcommand cmd: "rex-tab" }
            ] }
          }
          {
            name: normal_completion
            modifier: control
            keycode: char_x
            mode: [emacs vi_normal vi_insert helix_normal helix_select helix_insert]
            event: { until: [
              { send: menu name: completion_menu }
              { send: menunext }
              { edit: complete }
            ] }
          }
        ] }
      '';
    };

    starship = {
      enable = true;
      integrations.nushell.enable = true;

      settings = {
        command_timeout = 3000;
        format = "$hostname\${custom.host}$username$nix_shell$character";
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
            host = {
              description = "first label of the tailnet name, or the hostname";
              when = ''[ -n "$REX_SESSION" ] && [ -z "$SSH_CONNECTION" ]'';
              shell = [ "sh" ];
              command = ''
                n=$(rex --autostart=false whoami --json 2>/dev/null | jq -r '.tailscale.dns_name // empty' 2>/dev/null | cut -d. -f1)
                echo "''${n:-$(uname -n)}"
              '';
              style = "fg:black bg:blue";
              format = "[ ✦ $output ✦ ]($style)";
            };

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
                    if(self.contained_in("first_parent(@)"), "★"),
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
