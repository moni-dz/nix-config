# Replaces the Perl/bash activation scripts (users, /etc, stage 2) with nixos-core
{
  inputs,
  pkgs,
  ...
}:

{
  imports = [ inputs.nixos-core.nixosModules.default ];

  system.nixos-core = {
    enable = true;

    package = (pkgs.extend inputs.nixos-core.overlays.nixos-core).nixos-core.override {
      withStage1 = false;
      withInitScript = false;
      withPersistence = false;
    };

    components = {
      extraUtilsCommand.enable = false;
      bootStage1.enable = false;
      initialRamdisk.enable = false;
      bootloaderInstaller.enable = false;
      nixosInitCompat.enable = true;
    };
  };
}
