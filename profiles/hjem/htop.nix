{
  pkgs,
  ...
}:

{
  rum.programs = {
    htop = {
      enable = true;

      settings = {
        detailed_cpu_time = true;
        hide_kernel_threads = false;
        show_cpu_frequency = pkgs.stdenv.hostPlatform.isLinux;
        show_cpu_usage = true;
        show_program_path = false;
        show_thread_names = true;

        # PID USER PRIORITY NICE M_SIZE M_RESIDENT M_SHARE STATE PERCENT_CPU PERCENT_MEM TIME COMM
        fields = [
          0
          48
          17
          18
          39
          40
          41
          2
          46
          47
          49
          1
        ];
        left_meters = [
          "AllCPUs"
          "Memory"
          "Swap"
        ];
        left_meter_modes = [
          1
          1
          1
        ];
        right_meters = [
          "Tasks"
          "LoadAverage"
          "Uptime"
        ];
        right_meter_modes = [
          2
          2
          2
        ];
      };
    };
  };
}
