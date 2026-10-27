{
  disko.devices = {
    disk.main = {
      type = "disk";
      device = "/dev/nvme0n1";

      content = {
        type = "gpt";

        partitions = {
          ESP = {
            label = "ESP";
            size = "512M";
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
            end = "-8057M";
            priority = 2000;

            content = {
              type = "bcachefs";
              filesystem = "root";
            };
          };

          swap = {
            label = "swap";
            size = "100%";

            content.type = "swap";
          };
        };
      };
    };

    bcachefs_filesystems.root = {
      type = "bcachefs_filesystem";
      mountpoint = "/";
      uuid = "950685d4-3bd1-421a-a362-f74bf75907e7";
    };
  };
}
