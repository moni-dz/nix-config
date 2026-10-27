# NixOS and nix-darwin base environment.systemPackages
{ lib, pkgs, ... }:

{
  environment.systemPackages =
    __attrValues {
      inherit (pkgs)
        curl
        fd
        ripgrep
        man-pages-posix
        wget
        gitMinimal
        subversion
        ;

      gnu-coreutils =
        if pkgs.stdenv.hostPlatform.isLinux then pkgs.coreutils else pkgs.coreutils-prefixed;
    }
    ++ lib.optional pkgs.stdenv.hostPlatform.isLinux pkgs.man-pages;
}
