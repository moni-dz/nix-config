{ lib, ... }:

{
  boot.initrd = {
    # the stock list is for bare metal (ahci, nvme, usb, hid), hyper-v only needs its own modules
    includeDefaultModules = false;

    availableKernelModules = [
      "sd_mod"
      "sr_mod"
    ];
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  virtualisation.hypervGuest.enable = true;
}
