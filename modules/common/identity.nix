{ lib, ... }:

{
  options.identity = {
    name = lib.mkOption {
      type = lib.types.str;
      description = "Full name of the person this configuration belongs to, used for VCS authorship.";
    };

    email = lib.mkOption {
      type = lib.types.str;
      description = "Email address of the person this configuration belongs to, used for VCS authorship.";
    };
  };
}
