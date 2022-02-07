{
  sv = "systemctl";
  ga = "git add";
  gp = "git push";
  gr = "git rebase";
  gcm = "git commit";
  gpl = "git pull";
  gst = "git status";
  gck = "git checkout";
  grl = "git reflog";
  gdf = "git diff";
  mv = "mv -i";
  cp = "cp -i";
  rm = "rm -i";
  repl = ''. /etc/set-environment && nix repl "$(echo "$NIX_PATH" | perl -pe 's|.*(/nix/store/.*-source/repl.nix).*|\1|')" && exit'';
  swh = "home-manager switch --flake ~/.config/nix-config#fortuneteller2k";
  swn = "doas nixos-rebuild switch --flake ~/.config/nix-config#superfluous";
  cfg = "cd ~/.config/nix-config";
  bump = "cd ~/.config/nix-config && nix flake update --commit-lock-file --commit-lockfile-summary 'flake: bump flakes' && swn && swh";
}
