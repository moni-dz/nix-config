{
  disko.devices = {
    disk.main = {
      type = "disk";
      device = "/dev/sda";

      content = {
        type = "gpt";

        partitions = {
          ESP = {
            label = "ESP";
            size = "1G";
            type = "EF00";

            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [
                "fmask=0077"
                "dmask=0077"
              ];
            };
          };

          root = {
            label = "root";
            size = "100%";

            content = {
              type = "bcachefs";
              filesystem = "root";
            };
          };
        };
      };
    };

    bcachefs_filesystems.root = {
      type = "bcachefs_filesystem";
      mountpoint = "/";
      uuid = "ece3ba18-84ef-4763-85f5-bec59018e23c";
    };
  };
}
