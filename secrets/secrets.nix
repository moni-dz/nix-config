let
  fortuneteller2k =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHIw6Il9E+VR6oGwyb+f+/kA7vYciZaV4f1sWTBciAaj";
  superfluous =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICtfV+2+0M7U6KCnPMiJ63jHp05ucIF1jGvuMNFg6UlT";

  users = [ fortuneteller2k ];
  hosts = [ superfluous ];
in { "github-token.age".publicKeys = users ++ hosts; }
