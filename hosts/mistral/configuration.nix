{
  inputs,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix
    ./crowdsec.nix
    ./minecraft.nix
  ];

  boot = {
    # NOTE: replace this with your desired kernel, see: https://nixos.wiki/wiki/Linux_kernel for reference.
    kernelPackages = pkgs.linuxKernel.packages.linux_hardened;

    kernelParams = [
      "preempt=full"
      "quiet"
      "udev.log_level=3"
    ];

    loader.grub.device = "/dev/sda";
  };

  environment.systemPackages = [ pkgs.tmux ];

  nixpkgs.overlays = lib.mkOverride 10 [
    inputs.nix-minecraft.overlay
    inputs.crowdsec.overlays.default
  ];

  programs = {
    fish.enable = true;
    mosh.enable = true;
  };

  networking.firewall.allowedTCPPorts = [
    2022
    3306
    # 1433
    # 4747
    # 5432
  ];

  services = {
    dbus.implementation = "broker";

    eternal-terminal.enable = true;
    shpool.enable = true;

    openssh = {
      enable = true;
      settings = {
        GatewayPorts = "yes";
        PasswordAuthentication = false;
        PermitRootLogin = "yes";
      };
    };

    postgresql = {
      enable = false;
      package = pkgs.postgresql_17;
      enableTCPIP = true;
    };

    mysql = {
      enable = true;
      package = pkgs.mariadb_114;
    };
  };

  security.pam = {
    rssh.enable = true;
    services.sudo.rssh = true;
  };

  users.users.moni = {
    isNormalUser = true;
    home = "/home/moni";
    shell = pkgs.fish;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBlr5SHXali3ttWt8ygyWgCW2usWVsBhXebeyi2XKO2Z lythe1107@gmail.com"
    ];
  };
}
