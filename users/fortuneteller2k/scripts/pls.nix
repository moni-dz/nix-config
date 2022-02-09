''
  import os
  import sys


  def specify():
      sys.exit("Please specify either 'home' or 'nixos'.")


  def switch_nixos():
      if os.system("sudo -n nixos-rebuild switch --flake ~/.config/nix-config#superfluous") == 0:
          os.system("fd . /nix/var/nix/profiles -d 1 | tail -2 | xargs nvd diff")


  def switch_home():
      if os.system("home-manager switch --flake ~/.config/nix-config#fortuneteller2k") == 0:
          os.system("fd home-manager /nix/var/nix/profiles/per-user/fortuneteller2k -d 1 | tail -2 | xargs nvd diff")


  def pls(args):
      match args[1]:
          case "clean":
              os.system("sudo -n nix-collect-garbage -d")
          case "repl":
              os.system(". /etc/set-environment && nix repl \"$(echo \"$NIX_PATH\" | perl -pe 's|.*(/nix/store/.*-source/repl.nix).*|\\1|')\"")
          case "switch":
              match args[2]:
                  case "home":
                      switch_home()
                  case "nixos":
                      switch_nixos()
                  case "all":
                      switch_nixos()
                      switch_home()
                  case _:
                      specify()
          case "generations":
              match args[2]:
                  case "home": os.system("home-manager generations")
                  case "nixos": os.system("sudo -n nix-env -p /nix/var/nix/profiles/system --list-generations")
                  case _: specify()
          case "upgrade":
              os.system("cd ~/.config/nix-config && nix flake update --commit-lock-file --commit-lockfile-summary 'flake: bump flakes'")
              switch_nixos()
              switch_home()
              os.system("cd ~/.config/nix-config && git push")
          case _:
              sys.exit("pls: <switch/generations/clean/upgrade>")


  if __name__ == "__main__":
      pls(sys.argv)
''
