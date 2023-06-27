{ inputs, withSystem, ... }@ctx:

{
  # doesn't get merged for some reason...
  flake.homeConfigurations = {
    moni = import ./moni ctx;
    omni = import ./omni ctx;
    zero = import ./zero ctx;
  };
}
