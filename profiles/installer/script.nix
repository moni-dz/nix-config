{ pkgs }:

{
  targetSystem,
  diskoScript,
  user,
  passwordAge,
  hostKeyAge,
  masterPubkey,
  defaultDevice,
  hostName,
}:

pkgs.writeShellApplication {
  name = "install-host";

  runtimeInputs =
    __attrValues {
      inherit (pkgs)
        age
        coreutils
        gnused
        mkpasswd
        openssh
        ;
    }
    ++ pkgs.lib.optional pkgs.stdenv.hostPlatform.isLinux pkgs.util-linux;

  text =
    let
      values = {
        inherit
          targetSystem
          diskoScript
          user
          passwordAge
          hostKeyAge
          masterPubkey
          defaultDevice
          hostName
          ;
      };
    in
    builtins.replaceStrings (map (name: "@${name}@") (__attrNames values)) (map toString (
      __attrValues values
    )) (builtins.readFile ./install.sh);
}
