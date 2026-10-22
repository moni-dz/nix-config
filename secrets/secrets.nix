let
  moni = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBlr5SHXali3ttWt8ygyWgCW2usWVsBhXebeyi2XKO2Z";

  hands = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDSEtKx+3rpaqqahgigzvhkaxF7Y7DjOhxg7R3NVjGD9";

  users = [
    moni
  ];
in
{
  "tokens.age".publicKeys = users;
  "netrc.age".publicKeys = users ++ [ hands ];
}
