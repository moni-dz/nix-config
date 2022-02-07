''
  import os
  import sys


  def specify():
      sys.exit("Please specify either 'home' or 'nixos'.")


  def switch_nixos():
      if os.system("doas -n nixos-rebuild switch --flake ~/.config/nix-config#superfluous") == 0:
          os.system("ls -1 /nix/var/nix/profiles | tail -n 2 | awk '{print \"/nix/var/nix/profiles/\" $0}' - | xargs nvd diff")


  def switch_home():
      if os.system("home-manager switch --flake ~/.config/nix-config#fortuneteller2k") == 0:
          os.system("ls -1 /nix/var/nix/profiles/per-user/fortuneteller2k | tail -n 2 | awk '{print \"/nix/var/nix/profiles/per-user/fortuneteller2k/\" $0}' - | xargs nvd diff")


  def pls(args):
      match args[1]:
          case "clean":
              os.system("doas -n nix-collect-garbage -d")
          case "repl":
              os.system(". /etc/set-environment && nix repl \"$(echo \"$NIX_PATH\" | perl -pe 's|.*(/nix/store/.*-source/repl.nix).*|\\1|')\"")
          case "switch":
              match args[2]:
                  case "home": switch_home()
                  case "nixos": switch_nixos()
                  case _: specify()
          case "generations":
              match args[2]:
                  case "home": os.system("home-manager generations")
                  case "nixos": os.system("doas -n nix-env -p /nix/var/nix/profiles/system --list-generations")
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
