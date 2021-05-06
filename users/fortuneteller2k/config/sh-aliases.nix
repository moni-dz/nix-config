{
  cat = "bat";
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
  repl = ''. /etc/set-environment && nix repl "$(echo "$NIX_PATH" | perl -pe 's|.*(/nix/store/.*-source/repl.nix).*|\1|')"'';
}
