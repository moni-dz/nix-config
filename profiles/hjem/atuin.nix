_:

{
  rum.programs = {
    atuin = {
      enable = true;
      integrations.nushell.enable = true;

      settings = {
        style = "full";
        sync_frequency = "5m";
      };
    };
  };
}
