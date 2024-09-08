let
  moni = {
    darwin = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBlr5SHXali3ttWt8ygyWgCW2usWVsBhXebeyi2XKO2Z";
    linux = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOf7dkQDloUFN1Hxn/yWrcqMaJiH/jsXUGAAtL9l92xQ";
  };

  zero = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOf7dkQDloUFN1Hxn/yWrcqMaJiH/jsXUGAAtL9l92xQ";
  starcruiser = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKrPdqIiTrGqnN6eAhRuGl9ZV2sUz/IR85T3/TzUT4Ol";
  riscake = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEeZg4xxANKadIm8hnhM/rQrl77Xwwp0tFRnnANtFgI3";
  mistral = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFDRGyDQlHPogYIt0IIwI+/1D+U3qbOHOZOyPsAN2NWt";

  users = [
    moni.linux
    moni.darwin
    zero
  ];
in
{
  "tokens.age".publicKeys = users ++ [
    starcruiser
    riscake
  ];

  "ms-sql-server.age".publicKeys = users ++ [ mistral ];
}
