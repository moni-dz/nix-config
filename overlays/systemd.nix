final: prev: {
  systemd = pkgs.systemd.override = {
    withApparmor = false;
    withTimesyncd = false;
  };
}
