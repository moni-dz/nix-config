{ lib, ... }:

{
  boot.initrd.availableKernelModules = [
    "sd_mod"
    "sr_mod"
  ];

  fileSystems."/" = {
    device = "UUID=ece3ba18-84ef-4763-85f5-bec59018e23c";
    fsType = "bcachefs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/E32D-F93B";
    fsType = "vfat";
    options = [
      "fmask=0077"
      "dmask=0077"
    ];
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  virtualisation.hypervGuest.enable = true;
}
