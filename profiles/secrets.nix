{ config, self, ... }:

{
  age.rekey = {
    masterIdentities = [
      {
        identity = "/Users/moni/.ssh/id_ed25519";
        pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBlr5SHXali3ttWt8ygyWgCW2usWVsBhXebeyi2XKO2Z";
      }
    ];

    storageMode = "local";
    localStorageDir = self + "/secrets/rekeyed/${config.networking.hostName}";
  };
}
