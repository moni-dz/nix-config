# Show a closure diff against the running system whenever nixos-rebuild/darwin-rebuild activates a new one
{
  pkgs,
  inputs',
  ...
}:

let
  dix = inputs'.dix.packages.default;
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  diff = ''
    if [ -e /run/current-system ]; then
      echo "closure diff:" >&2
      ${dix}/bin/dix diff /run/current-system "$systemConfig" >&2 || true
    fi
  '';
in
{
  system.activationScripts =
    if isDarwin then
      { preActivation.text = diff; }
    else
      {
        dix = {
          supportsDryActivation = true;
          text = diff;
        };
      };
}
