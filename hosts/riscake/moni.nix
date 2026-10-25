{ pkgs, ... }:

{
  users.users.moni = {
    name = "moni";
    home = "/Users/moni";
    shell = pkgs.nushell;
  };

  # nix-darwin only applies `shell` to users it owns (`users.knownUsers`), which would also let it
  # delete the account, so set the login shell on the existing user ourselves
  system.activationScripts.postActivation.text = ''
    shell=/run/current-system/sw/bin/nu
    if [ "$(dscl . -read /Users/moni UserShell | awk '{print $2}')" != "$shell" ]; then
      echo "setting moni's login shell to $shell" >&2
      dscl . -create /Users/moni UserShell "$shell"
    fi
  '';

  # NOTE: change or remove this if you are not me, the secrets are encrypted to my keys.
  age = {
    identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    rekey.hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBJ5+EqB0hCxbEVqjh9DwROMOBNXsp40H7qWe8Bscxb/";

    secrets.tokens = {
      rekeyFile = ../../secrets/tokens.age;
      owner = "moni";
      mode = "600";
    };

    # `access-tokens = github.com=...`, included from nix.custom.conf, see `determinateNix` in ./system.nix
    secrets.nix-access-tokens = {
      rekeyFile = ../../secrets/nix-access-tokens.age;
      owner = "moni";
      mode = "600";
    };

    # read by determinate-nixd (root), see `determinateNix` in ./system.nix
    secrets.netrc = {
      rekeyFile = ../../secrets/netrc.age;
      owner = "root";
      mode = "600";
    };
  };

  hjem.users.moni = {
    enable = true;
    imports = [
      (
        {
          config,
          pkgs,
          osConfig,
          self',
          ...
        }:

        {
          imports = [ ../../profiles/hjem ];

          packages = __attrValues {
            inherit (pkgs) iproute2mac watchman;
          };

          rum.programs = {
            jujutsu.package = self'.packages.jujutsu;

            nushell.envFile = ''
              open --raw ${osConfig.age.secrets.tokens.path} | lines | parse "export {key}={value}" | reduce -f {} {|it, acc| $acc | upsert $it.key $it.value } | load-env
              $env.BUN_INSTALL = ($env.HOME | path join ".bun")
              $env.path ++= ["/etc/profiles/per-user/${config.user}/bin" "/run/current-system/sw/bin" "/usr/bin" "/usr/local/bin" "/opt/homebrew/bin" "${config.directory}/.modular/bin" "/opt/local/bin" "/opt/homebrew/opt/llvm/bin" "${config.directory}/.local/share/modular/pkg/packages.modular.com_mojo/bin" ($env.BUN_INSTALL | path join "bin") "/Users/Shared/DBngin/postgresql/17.0/bin" "${config.directory}/.local/bin" "${config.directory}/.cargo/bin" "/Applications/OrbStack.app/Contents/MacOS/bin" "/Applications/OrbStack.app/Contents/MacOS/xbin" "/nix/var/nix/profiles/default/bin"]
            '';
          };
        }
      )
    ];
  };
}
