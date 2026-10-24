{
  config,
  lib,
  self,
  ...
}:

let
  me = config.networking.hostName;

  users = {
    hands = "lyt";
    riscake = "moni";
  };

  hosts = self.nixosConfigurations // self.darwinConfigurations;
  peers = removeAttrs hosts [ me ];

  pubkey = host: lib.removeSuffix "\n" (builtins.readFile (self + "/secrets/builders/${host}.pub"));
in
{
  age.secrets.builder-key = {
    rekeyFile = self + "/secrets/builders/${me}.age";
    generator.script =
      {
        lib,
        file,
        pkgs,
        ...
      }:
      ''
        dir=$(mktemp -d)
        ${pkgs.openssh}/bin/ssh-keygen -q -t ed25519 -N "" -C ${lib.escapeShellArg "builder@${me}"} -f "$dir/key"
        mv "$dir/key.pub" ${lib.escapeShellArg (lib.removeSuffix ".age" file + ".pub")}
        cat "$dir/key"
        rm -rf "$dir"
      '';
  };

  services.openssh.enable = true;

  users.users.${users.${me}}.openssh.authorizedKeys.keys = lib.mapAttrsToList (
    host: _: pubkey host
  ) peers;

  programs.ssh.knownHosts = lib.mapAttrs (_: peer: {
    publicKey = peer.config.age.rekey.hostPubkey;
  }) peers;

  nix.settings = {
    builders-use-substitutes = true;

    # ew, but determinate nix compatibility
    # ssh-ng://user@host platforms key max-jobs speed features mandatory-features
    builders = lib.mkForce (
      lib.concatStringsSep " ; " (
        lib.mapAttrsToList (
          host: peer:
          lib.concatStringsSep " " [
            "ssh-ng://${users.${host}}@${host}"
            peer.config.nixpkgs.hostPlatform.system
            config.age.secrets.builder-key.path
            "8"
            "1"
            "big-parallel,benchmark"
            "-"
          ]
        ) peers
      )
    );
  };
}
