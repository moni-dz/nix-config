ctx:

{
  flake.homeConfigurations = {
    moni = import ./moni ctx;
    omni = import ./omni ctx;
    zero = import ./zero ctx;
  };
}
