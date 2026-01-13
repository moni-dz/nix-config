{
  config,
  pkgs,
  lib,
  infuse,
  ...
}:

let
  kernel = pkgs.pkgsLLVM.linuxKernel.kernels.linux_xanmod_latest;

  xanmod_experiments = kernel.override {
    ignoreConfigErrors = true;

    extraMakeFlags = kernel.extraMakeFlags ++ [
      "LLVM=1"
      "LLVM_IAS=1"
    ];

    argsOverride.structuredExtraConfig =
      with lib.kernel;
      kernel.structuredExtraConfig
      // {
        # LLVM's ThinLTO
        LTO = lib.mkForce yes;
        LTO_CLANG = lib.mkForce yes;
        LTO_CLANG_THIN = lib.mkForce yes;

        # don't do this...
        CPU_MITIGATIONS = lib.mkForce no;

        # Raptor Lake
        GENERIC_CPU = lib.mkForce no;
        MRAPTORLAKE = lib.mkForce yes;

        # unused features
        NET_VENDOR_3COM = lib.mkForce no;
        NET_VENDOR_ADAPTEC = lib.mkForce no;
        NET_VENDOR_AGERE = lib.mkForce no;
        NET_VENDOR_ALACRITECH = lib.mkForce no;
        NET_VENDOR_ALTEON = lib.mkForce no;
        NET_VENDOR_AMAZON = lib.mkForce no;
        NET_VENDOR_AMD = lib.mkForce no;
        NET_VENDOR_AQUANTIA = lib.mkForce no;
        NET_VENDOR_ARC = lib.mkForce no;
        NET_VENDOR_ASIX = lib.mkForce no;
        NET_VENDOR_ATHEROS = lib.mkForce no;
        NET_VENDOR_BROADCOM = lib.mkForce no;
        NET_VENDOR_CADENCE = lib.mkForce no;
        NET_VENDOR_CAVIUM = lib.mkForce no;
        NET_VENDOR_CHELSIO = lib.mkForce no;
        NET_VENDOR_CISCO = lib.mkForce no;
        NET_VENDOR_CORTINA = lib.mkForce no;
        NET_VENDOR_DAVICOM = lib.mkForce no;
        NET_VENDOR_DEC = lib.mkForce no;
        NET_VENDOR_DLINK = lib.mkForce no;
        NET_VENDOR_EMULEX = lib.mkForce no;
        NET_VENDOR_ENGLEDER = lib.mkForce no;
        NET_VENDOR_EZCHIP = lib.mkForce no;
        NET_VENDOR_FUNGIBLE = lib.mkForce no;
        NET_VENDOR_GOOGLE = lib.mkForce no;
        NET_VENDOR_HISILICON = lib.mkForce no;
        NET_VENDOR_HUAWEI = lib.mkForce no;
        NET_VENDOR_I825XX = lib.mkForce no;
        NET_VENDOR_ADI = lib.mkForce no;
        NET_VENDOR_LITEX = lib.mkForce no;
        NET_VENDOR_MARVELL = lib.mkForce no;
        NET_VENDOR_MELLANOX = lib.mkForce no;
        NET_VENDOR_META = lib.mkForce no;
        NET_VENDOR_MICREL = lib.mkForce no;
        NET_VENDOR_MICROCHIP = lib.mkForce no;
        NET_VENDOR_MICROSEMI = lib.mkForce no;
        NET_VENDOR_MICROSOFT = lib.mkForce no;
        NET_VENDOR_MYRI = lib.mkForce no;
        NET_VENDOR_NI = lib.mkForce no;
        NET_VENDOR_NATSEMI = lib.mkForce no;
        NET_VENDOR_NETERION = lib.mkForce no;
        NET_VENDOR_NETRONOME = lib.mkForce no;
        NET_VENDOR_8390 = lib.mkForce no;
        NET_VENDOR_NVIDIA = lib.mkForce no;
        NET_VENDOR_OKI = lib.mkForce no;
        NET_VENDOR_PACKET_ENGINES = lib.mkForce no;
        NET_VENDOR_PENSANDO = lib.mkForce no;
        NET_VENDOR_QLOGIC = lib.mkForce no;
        NET_VENDOR_BROCADE = lib.mkForce no;
        NET_VENDOR_QUALCOMM = lib.mkForce no;
        NET_VENDOR_RDC = lib.mkForce no;
        NET_VENDOR_RENESAS = lib.mkForce no;
        NET_VENDOR_ROCKER = lib.mkForce no;
        NET_VENDOR_SAMSUNG = lib.mkForce no;
        NET_VENDOR_SEEQ = lib.mkForce no;
        NET_VENDOR_SILAN = lib.mkForce no;
        NET_VENDOR_SIS = lib.mkForce no;
        NET_VENDOR_SOLARFLARE = lib.mkForce no;
        NET_VENDOR_SMSC = lib.mkForce no;
        NET_VENDOR_SOCIONEXT = lib.mkForce no;
        NET_VENDOR_STMICRO = lib.mkForce no;
        NET_VENDOR_SUN = lib.mkForce no;
        NET_VENDOR_SYNOPSYS = lib.mkForce no;
        NET_VENDOR_TEHUTI = lib.mkForce no;
        NET_VENDOR_TI = lib.mkForce no;
        NET_VENDOR_VERTEXCOM = lib.mkForce no;
        NET_VENDOR_VIA = lib.mkForce no;
        NET_VENDOR_WANGXUN = lib.mkForce no;
        NET_VENDOR_WIZNET = lib.mkForce no;
        NET_VENDOR_XILINX = lib.mkForce no;
        WLAN_VENDOR_ADMTEK = lib.mkForce no;
        WLAN_VENDOR_ATH = lib.mkForce no;
        WLAN_VENDOR_ATMEL = lib.mkForce no;
        WLAN_VENDOR_BROADCOM = lib.mkForce no;
        WLAN_VENDOR_INTERSIL = lib.mkForce no;
        WLAN_VENDOR_MARVELL = lib.mkForce no;
        WLAN_VENDOR_MEDIATEK = lib.mkForce no;
        WLAN_VENDOR_MICROCHIP = lib.mkForce no;
        WLAN_VENDOR_PURELIFI = lib.mkForce no;
        WLAN_VENDOR_RALINK = lib.mkForce no;
        WLAN_VENDOR_REALTEK = lib.mkForce no;
        WLAN_VENDOR_RSI = lib.mkForce no;
        WLAN_VENDOR_SILABS = lib.mkForce no;
        WLAN_VENDOR_ST = lib.mkForce no;
        WLAN_VENDOR_TI = lib.mkForce no;
        WLAN_VENDOR_ZYDAS = lib.mkForce no;
        WLAN_VENDOR_QUANTENNA = lib.mkForce no;
        DRM_RADEON = lib.mkForce no;
        DRM_AMDGPU = lib.mkForce no;
        DRM_NOUVEAU = lib.mkForce no;
        DRM_NOVA = lib.mkForce no;
        DRM_VGEM = lib.mkForce no;
        DRM_VKMS = lib.mkForce no;
        DRM_VMWGFX = lib.mkForce no;
        DRM_GMA500 = lib.mkForce no;
        DRM_UDL = lib.mkForce no;
        DRM_AST = lib.mkForce no;
        DRM_MGAG200 = lib.mkForce no;
        DRM_QXL = lib.mkForce no;
        DRM_VIRTIO_GPU = lib.mkForce no;
        DRM_BOCHS = lib.mkForce no;
        DRM_CIRRUS_QEMU = lib.mkForce no;
        DRM_VBOXVIDEO = lib.mkForce no;
        ACERHDF = lib.mkForce no;
        ACER_WMI = lib.mkForce no;
        ALIENWARE_WMI = lib.mkForce no;
        DELL_SMBIOS = lib.mkForce no;
        FUJITSU_LAPTOP = lib.mkForce no;
        HP_ACCEL = lib.mkForce no;
        HP_WMI = lib.mkForce no;
        IDEAPAD_LAPTOP = lib.mkForce no;
        THINKPAD_ACPI = lib.mkForce no;
        MSI_LAPTOP = lib.mkForce no;
        SAMSUNG_LAPTOP = lib.mkForce no;
        TOSHIBA_BT_RFKILL = lib.mkForce no;
        TOSHIBA_HAPS = lib.mkForce no;
        SONY_LAPTOP = lib.mkForce no;
        PANASONIC_LAPTOP = lib.mkForce no;
        COMPAL_LAPTOP = lib.mkForce no;
        TOPSTAR_LAPTOP = lib.mkForce no;
        EDAC_DECODE_MCE = lib.mkForce no;
        EDAC_E752X = lib.mkForce no;
        EDAC_I82975X = lib.mkForce no;
        EDAC_I3000 = lib.mkForce no;
        EDAC_I3200 = lib.mkForce no;
        EDAC_IE31200 = lib.mkForce no;
        EDAC_X38 = lib.mkForce no;
        EDAC_I5400 = lib.mkForce no;
        EDAC_I7CORE = lib.mkForce no;
        EDAC_I5100 = lib.mkForce no;
        EDAC_I7300 = lib.mkForce no;
        EDAC_SBRIDGE = lib.mkForce no;
        EDAC_SKX = lib.mkForce no;
        EDAC_I10NM = lib.mkForce no;
        EDAC_PND2 = lib.mkForce no;
        EDAC_IGEN6 = lib.mkForce no;
        HAMRADIO = lib.mkForce no;
        ISDN = lib.mkForce no;
        NTB = lib.mkForce no;
        IPACK_BUS = lib.mkForce no;
        PCCARD = lib.mkForce no;
        RAPIDIO = lib.mkForce no;
        PC104 = lib.mkForce no;
        GREYBUS = lib.mkForce no;
        COMEDI = lib.mkForce no;
        STAGING_MEDIA = lib.mkForce no;
        VME_BUS = lib.mkForce no;
        GPIB = lib.mkForce no;
        FIREWIRE = lib.mkForce no;
        FIREWIRE_NOSY = lib.mkForce no;
      };
  };

  linuxPackages_xanmod_experiments = pkgs.linuxPackagesFor xanmod_experiments;
in
{
  boot.kernelPackages = lib.mkForce linuxPackages_xanmod_experiments;
  hardware.nvidia.nvidiaSettings = false;

  /*
    still fails with stripping, it can't find the strip binary
    hardware.nvidia.package = lib.mkForce (infuse config.boot.kernelPackages.nvidiaPackages.beta {
      __output.passthru.settings.__output.makeFlags.__append = [
        "STRIP=${pkgs.llvmPackages.bintools-unwrapped}/bin/strip"
      ];
    });
  */
}
