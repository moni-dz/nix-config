let
  fortuneteller2k = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFcycVYoYUln0bNU1OeV/tW/Zp/wFCIPNqa3Rc5o0vFD";
  starcruiser = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILUA2hHp25/ou4Drj1w0OQDbNKTJMZHVhDUgvyCWeguC";
  superfluous = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICtfV+2+0M7U6KCnPMiJ63jHp05ucIF1jGvuMNFg6UlT";

  users = [ fortuneteller2k ];
  hosts = [ starcruiser superfluous ];
in
{
  "github-token.age".publicKeys = users ++ hosts;
}
