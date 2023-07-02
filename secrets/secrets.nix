let
  moni = {
    darwin = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBlr5SHXali3ttWt8ygyWgCW2usWVsBhXebeyi2XKO2Z";
    linux = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOf7dkQDloUFN1Hxn/yWrcqMaJiH/jsXUGAAtL9l92xQ";
  };

  zero = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOf7dkQDloUFN1Hxn/yWrcqMaJiH/jsXUGAAtL9l92xQ";
  starcruiser = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKrPdqIiTrGqnN6eAhRuGl9ZV2sUz/IR85T3/TzUT4Ol";
  shaker = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEeZg4xxANKadIm8hnhM/rQrl77Xwwp0tFRnnANtFgI3";

  users = [ moni.linux moni.darwin zero ];
  hosts = [ starcruiser shaker ];
in
{
  "github-token.age".publicKeys = users ++ hosts;
}
