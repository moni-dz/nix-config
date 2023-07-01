_:

{
  parts.homeConfigurations.zero = {
    system = "x86_64-linux";
    stateVersion = "21.11";
    modules = [ ./home.nix ];
  };
}
