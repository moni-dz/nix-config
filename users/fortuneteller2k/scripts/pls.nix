''
  from subprocess import run
  from signal import SIGINT
  import sys


  def help():
      sys.exit("pls: <switch/generations/clean/upgrade>")


  def specify():
      sys.exit("Please specify either 'home' or 'nixos'.")


  def get_diff(path):
      return run("exa -1 " + path + " | tail -n 2 | awk '{print \"" + path + "/\" $1}' - | xargs nvd diff", shell=True).returncode


  def switch_nixos():
      try:
          process = run("sudo -n nixos-rebuild switch --flake ~/.config/nix-config#starcruiser", shell=True)

          if process.returncode == 0:
              return get_diff("/nix/var/nix/profiles")
          else:
              return process.returncode
      except KeyboardInterrupt:
          return SIGINT


  def switch_home():
      try:
          process = run("home-manager switch --flake ~/.config/nix-config#fortuneteller2k", shell=True)

          if process.returncode == 0:
              return get_diff("/nix/var/nix/profiles/per-user/fortuneteller2k")
          else:
              return process.returncode
      except KeyboardInterrupt:
          return SIGINT


  def switch_all():
      retcode = switch_nixos()

      if retcode == 0:
          switch_home()
      else:
          sys.exit(retcode)


  def upgrade():
      try:
          run("cd ~/.config/nix-config && nix flake update --commit-lock-file --commit-lockfile-summary 'flake: bump flakes'", shell=True)
          switch_all()
          run("cd ~/.config/nix-config && git push", shell=True)
      except KeyboardInterrupt:
          sys.exit(SIGINT)


  def pls(args):
      if len(args) < 2:
          help()

      match args[1]:
          case "clean":
              run("sudo -n nix-collect-garbage -d", shell=True)
          case "repl":
              run(". /etc/set-environment && nix repl \"$(echo \"$NIX_PATH\" | perl -pe 's|.*(/nix/store/.*-source/repl.nix).*|\\1|')\"", shell=True)
          case "switch":
              match args[2]:
                  case "home": switch_home()
                  case "nixos": switch_nixos()
                  case "all": switch_all()
                  case _: specify()
          case "generations":
              match args[2]:
                  case "home": run("home-manager generations", shell=True)
                  case "nixos": run("sudo -n nix-env -p /nix/var/nix/profiles/system --list-generations", shell=True)
                  case _: specify()
          case "upgrade":
              upgrade()
          case _:
              help()


  if __name__ == "__main__":
      pls(sys.argv)
''
