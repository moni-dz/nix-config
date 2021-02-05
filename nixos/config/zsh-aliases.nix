{
  ls = "exa --icons";
  la = "exa --icons -la";
  l = "exa --icons -l";
  sv = "systemctl";
  ga = "git add";
  gp = "git push";
  gr = "git rebase";
  gam = "git amend";
  gcm = "git commit";
  gpl = "git pull";
  gst = "git status";
  "nix repl" = 
    "source /etc/set-environment && nix repl $(echo $NIX_PATH | perl -pe 's|.*(/nix/store/.*-source/repl.nix).*|\1|')";
} 
