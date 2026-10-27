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
    __attrValues { inherit (pkgs) age coreutils gnused mkpasswd openssh; }
    ++ pkgs.lib.optional pkgs.stdenv.hostPlatform.isLinux pkgs.util-linux;

  text = builtins.readFile (
    pkgs.replaceVars ./install.sh {
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
    }
  );
}
