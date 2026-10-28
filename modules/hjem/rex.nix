{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    concatMapStringsSep
    mkEnableOption
    mkIf
    mkMerge
    optional
    ;

  cfg = config.rum.programs.rex;

  initSources =
    optional cfg.integrations.smart-splits.enable ./rex/smart-splits.fnl
    ++ optional cfg.integrations.agent-reaper.enable ./rex/reap-all-agents.fnl;

  agentReaper =
    pkgs.runCommand "agent-reaper.lua" { nativeBuildInputs = [ pkgs.luaPackages.fennel ]; }
      ''
        fennel --globals rex --compile ${./rex/agent-reaper.fnl} > $out
      '';

  # fennel-ls reads the rex API's docs from here; see rex/flsproject.fnl
  rexDocset = pkgs.runCommand "rex-docset.lua" { nativeBuildInputs = [ pkgs.luaPackages.fennel ]; } ''
    fennel --compile ${./rex/types/rex-docset.fnl} > $out
  '';

  # Each compiled chunk runs as a function of its own: the compiler can end one with
  # `return`, which would otherwise end the whole file and skip the chunks after it.
  initLua = pkgs.runCommand "rex-init.lua" { nativeBuildInputs = [ pkgs.luaPackages.fennel ]; } ''
    : > $out
    ${concatMapStringsSep "\n" (
      src:
      "{ echo 'do local function chunk()'; fennel --globals rex --compile ${src}; echo 'end chunk() end'; } >> $out"
    ) initSources}
  '';
in
{
  options.rum.programs.rex.integrations = {
    nushell.enable = mkEnableOption "Rex tab completion keybindings for nushell";
    starship.enable = mkEnableOption "Rex host segment for starship";
    smart-splits.enable = mkEnableOption "Rex key bindings for smart-splits.nvim, pane navigation and resizing across Neovim";
    types.enable = mkEnableOption "the fennel-ls docset for Rex's Lua API, so the Fennel files that script Rex get completion and hover";
    agent-reaper.enable = mkEnableOption "Rex actions that start agent-reaper.lua, which closes the panes and scratch sessions that agents leave behind, and install it to ~/.claude/bin";
  };

  config = mkMerge [
    (mkIf (initSources != [ ]) {
      xdg.config.files."rex/init.lua".source = initLua;
    })

    (mkIf cfg.integrations.types.enable {
      xdg.data.files."fennel-ls/docsets/rex.lua".source = rexDocset;
    })

    # the "Reap All Agents" action runs this script, and CLAUDE.md names the path
    (mkIf cfg.integrations.agent-reaper.enable {
      files.".claude/bin/agent-reaper.lua".source = agentReaper;
    })

    (mkIf (cfg.integrations.nushell.enable && config.rum.programs.nushell.enable) {
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

    (mkIf (cfg.integrations.starship.enable && config.rum.programs.starship.enable) {
      rum.programs.starship.settings.custom.host = {
        description = "first label of the tailnet name, or the hostname";
        when = ''[ -n "$REX_SESSION" ] && [ -z "$SSH_CONNECTION" ]'';
        shell = [ "sh" ];
        command = ''
          n=$(rex --autostart=false whoami --json 2>/dev/null | jq -r '.tailscale.dns_name // empty' 2>/dev/null | cut -d. -f1)
          echo "''${n:-$(uname -n)}"
        '';
        style = "fg:black bg:blue";
        format = "[ ⌂ $output ]($style)";
      };
    })
  ];
}
