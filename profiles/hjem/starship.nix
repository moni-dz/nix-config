{
  config,
  ...
}:

{
  rum.programs = {
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
  };
}
