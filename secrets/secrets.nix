let
  moni-linux = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOf7dkQDloUFN1Hxn/yWrcqMaJiH/jsXUGAAtL9l92xQ";
  moni-darwin = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBlr5SHXali3ttWt8ygyWgCW2usWVsBhXebeyi2XKO2Z";
  zero = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOf7dkQDloUFN1Hxn/yWrcqMaJiH/jsXUGAAtL9l92xQ";
  starcruiser = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKrPdqIiTrGqnN6eAhRuGl9ZV2sUz/IR85T3/TzUT4Ol";
  superfluous = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICtfV+2+0M7U6KCnPMiJ63jHp05ucIF1jGvuMNFg6UlT";

  users = [ moni-linux moni-darwin zero ];
  hosts = [ starcruiser superfluous ];
in
{
  "github-token.age".publicKeys = users ++ hosts;
}
