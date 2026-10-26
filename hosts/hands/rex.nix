{
  services.rex.loginServer = {
    enable = true;
    accounts.lyt.principals = [ "tailscale:moni-dz@github" ];
    tailscaleServe.enable = true;
  };
}
