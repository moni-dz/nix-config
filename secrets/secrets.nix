let
  moni = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBlr5SHXali3ttWt8ygyWgCW2usWVsBhXebeyi2XKO2Z";

  users = [
    moni
  ];
in
{
  "tokens.age".publicKeys = users;
}
