{
  services.rex.loginServer = {
    enable = true;
    accounts.moni.principals = [ "tailscale:moni-dz@github" ];
    tailscaleServe.enable = true;
  };
}
