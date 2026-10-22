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
    };
  };
}
