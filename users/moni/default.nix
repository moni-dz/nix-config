{
  imports = [ ./age.nix ];

  users.users.moni = {
    name = "moni";
    home = "/Users/moni";
  };

  hjem.users.moni = {
    enable = true;
    imports = [ ./hjem.nix ];
  };
}
