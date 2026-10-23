{
  config,
  lib,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf mkMerge;

  cfg = config.rum.programs.rex;
in
{
  options.rum.programs.rex.integrations = {
    nushell.enable = mkEnableOption "Rex tab completion keybindings for nushell";
    starship.enable = mkEnableOption "Rex host segment for starship";
    smart-splits.enable = mkEnableOption "Rex key bindings for smart-splits.nvim, pane navigation and resizing across Neovim";
  };

  config = mkMerge [
    (mkIf cfg.integrations.smart-splits.enable {
      xdg.config.files."rex/init.lua".source = ./rex/smart-splits.lua;
    })

    (mkIf cfg.integrations.nushell.enable {
      rum.programs.nushell.extraConfig = ''
        def rex-tab [] {
          let before = (commandline | str substring 0..<(commandline get-cursor))
          let word = ($before | split row -r '\s+' | last | default "")
          if ($word =~ '^(~|\.{1,2}$|.*/)') {
            commandline edit --replace $before  # keep the typed line; insert path goes after the cursor
            # background job keeps nu in the foreground: Rex resolves the popover's directory
            # from the foreground process, and a just-exiting rex child makes it fall back to ~
            job spawn { rex do pane.insert_path } | ignore
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
    })

    (mkIf cfg.integrations.starship.enable {
      rum.programs.starship.settings.custom.host = {
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
    })
  ];
}
