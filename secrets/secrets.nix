let
  moni = {
    darwin = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBlr5SHXali3ttWt8ygyWgCW2usWVsBhXebeyi2XKO2Z";
    linux = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOf7dkQDloUFN1Hxn/yWrcqMaJiH/jsXUGAAtL9l92xQ";
  };

  zero = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOf7dkQDloUFN1Hxn/yWrcqMaJiH/jsXUGAAtL9l92xQ";
  starcruiser = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKrPdqIiTrGqnN6eAhRuGl9ZV2sUz/IR85T3/TzUT4Ol";
  riscake = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEeZg4xxANKadIm8hnhM/rQrl77Xwwp0tFRnnANtFgI3";
  mistral = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP1EZ5HhV78BkmMNy8FGULpbG2abIrrf2g82jdjEIOdm";

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

  "crowdsec.age".publicKeys = users ++ [ mistral ];
  "bouncer.age".publicKeys = users ++ [ mistral ];
}
