{ config, ... }:

{
  # NOTE: change or remove this if you are not me, the secret is encrypted to my keys.
  age = {
    identityPaths = [
      "${config.users.users.moni.home}/.ssh/id_ed25519"
      "/etc/ssh/ssh_host_ed25519_key"
    ];

    secrets.tokens = {
      file = ../../secrets/tokens.age;
      owner = "moni";
      mode = "600";
    };

    # read by determinate-nixd (root), see `determinateNix` in hosts/riscake
    secrets.netrc = {
      file = ../../secrets/netrc.age;
      owner = "root";
      mode = "600";
    };
  };
}
