{ pkgs, ... }:

{
  imports = [ ./age.nix ];

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

  hjem.users.moni = {
    enable = true;
    imports = [ ./hjem.nix ];
  };
}
