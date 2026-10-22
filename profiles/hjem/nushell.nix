_:

{
  rum.programs = {
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
  };
}
