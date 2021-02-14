rec {
  lightModeEnabled = false;
  colors = if lightModeEnabled then (import ./colors-light.nix) else (import ./colors.nix);
}
