{
  cat = "bat";
  ls = "exa --icons";
  la = "exa --icons -la";
  l = "exa --icons -l";
  sv = "systemctl";
  ga = "git add";
  gp = "git push";
  gr = "git rebase";
  gcm = "git commit";
  gpl = "git pull";
  gst = "git status";
  gck = "git checkout";
  grl = "git reflog";
  mv = "mv -i";
  cp = "cp -i";
  rm = "rm -i";
  repl = ''. /etc/set-environment && nix repl "$(echo "$NIX_PATH" | perl -pe 's|.*(/nix/store/.*-source/repl.nix).*|\1|')"'';
  hammer = "nix run github:jtojnar/nixpkgs-hammering";
}
