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

        tree_view = true;

        # PID USER M_SIZE STATE PERCENT_CPU PERCENT_MEM TIME COMM
        fields = [
          0
          48
          39
          2
          46
          47
          49
          1
        ];

        # split CPUs across both columns so high core counts stay readable
        left_meters = [
          "LeftCPUs8"
          "Memory"
          "Swap"
        ];
        left_meter_modes = [
          1
          1
          1
        ];
        right_meters = [
          "RightCPUs8"
          "Tasks"
          "LoadAverage"
          "Uptime"
        ];
        right_meter_modes = [
          1
          2
          2
          2
        ];
      };
    };
  };
}
