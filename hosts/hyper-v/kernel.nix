{ lib, pkgs, ... }:

let
  off = names: lib.genAttrs names (_: lib.mkForce lib.kernel.no);

  strip = [
    # wireless, audio, media, bluetooth
    "SOUND"
    "CFG80211"
    "MAC80211"
    "BT"
    "NFC"
    "MEDIA_SUPPORT"
    "WWAN"
    "HAMRADIO"
    "CAN"
    "INFINIBAND"

    # physical nics, hv_netvsc doesn't need ETHERNET
    "ETHERNET"

    # physical buses
    "USB_SUPPORT"
    "THUNDERBOLT"
    "PCCARD"
    "FIREWIRE"
    "PARPORT"
    "MTD"
    "SPI"
    "IIO"
    "X86_PLATFORM_DEVICES"
    "STAGING"

    # storage
    "ATA"
    "BLK_DEV_NVME"
    "FUSION"
    "MEGARAID_SAS"
    "SCSI_AACRAID"
    "SCSI_HPSA"
    "SCSI_SMARTPQI"
    "SCSI_MPT3SAS"
    "SCSI_QLA_FC"
    "SCSI_LPFC"
    "SCSI_UFSHCD"
    "ISCSI_TCP"

    # drm drivers
    "DRM_ACCEL"
    "DRM_AMDGPU"
    "DRM_RADEON"
    "DRM_NOUVEAU"
    "DRM_I915"
    "DRM_XE"
    "DRM_VMWGFX"
    "DRM_QXL"
    "DRM_VIRTIO_GPU"
    "DRM_BOCHS"
    "DRM_CIRRUS_QEMU"
    "DRM_GMA500"
    "DRM_AST"
    "DRM_MGAG200"

    # tracing and suspend
    "FTRACE"
    "KPROBES"
    "HIBERNATION"

    # board and laptop plumbing
    "COMEDI"
    "PWM"
    "MMC"
    "EDAC"
    "CPU_FREQ"
    "WATCHDOG"

    # input devices
    "INPUT_JOYSTICK"
    "INPUT_TABLET"
    "INPUT_TOUCHSCREEN"

    # virtualization
    "KVM"
    "XEN"
    "VFIO"
    "VHOST_MENU"

    # filesystems
    "BTRFS_FS"
    "XFS_FS"
    "F2FS_FS"
    "JFS_FS"
    "NFS_FS"
    "NFSD"
    "CIFS"
    "SMB_SERVER"
    "NTFS3_FS"
    "EXFAT_FS"
    "UDF_FS"
    "CEPH_FS"
    "OCFS2_FS"
    "GFS2_FS"
    "NILFS2_FS"
    "ECRYPT_FS"

    # networking protocols
    "NET_DSA"
    "BATMAN_ADV"
    "TIPC"
    "RDS"
    "ATM"
    "OPENVSWITCH"
    "IP_VS"
    "IEEE802154"
    "6LOWPAN"
  ];

  words =
    text: lib.filter (w: w != "") (lib.splitString " " (lib.replaceStrings [ "\n" ] [ " " ] text));

  loaded = words ''
    aead aesni_intel af_packet atkbd autofs4 cdrom configfs dm_mod efivarfs evdev fat
    fuse gf128mul hid hid_generic hid_hyperv hv_balloon hv_netvsc hv_sock hv_storvsc
    hv_utils hv_vmbus hyperv_drm hyperv_keyboard ip6t_rpfilter ipt_rpfilter libchacha
    libpoly1305 libps2 loop lz4hc_compress mousedev nf_conntrack nf_defrag_ipv4
    nf_defrag_ipv6 nf_nat nf_tables nft_chain_nat nft_compat nft_ct nft_fib
    nft_fib_inet nft_fib_ipv4 nft_fib_ipv6 nls_cp437 nls_iso8859_1 nls_utf8 ptp
    pps_core raid6_pq rfkill rtc_cmos sch_fq_codel scsi_common scsi_mod
    scsi_transport_fc sd_mod serio serio_raw sr_mod tun unix_diag vfat vmgenid vsock
    xor x_tables xt_connmark xt_conntrack xt_mark xt_MASQUERADE xt_pkttype xt_tcpudp
  '';

  onDemand = words ''
    overlay squashfs erofs isofs ext4 jbd2 veth bridge br_netfilter dummy wireguard
    nft_masq nft_reject nft_reject_inet nft_counter nft_limit nft_log nf_reject_ipv4
    nf_reject_ipv6 nf_log_syslog sch_fq tcp_bbr chacha_generic poly1305_generic
    chacha20poly1305 xxhash_generic crc32c_generic crc32c_intel crc32_pclmul
    sha256_generic sha512_generic zstd lz4 cryptd
  '';

  modules = loaded ++ onDemand;
  kernel = pkgs.linuxPackages_latest.kernel;
  llvm = pkgs.llvmPackages;

  stdenv = pkgs.overrideCC llvm.stdenv (llvm.clang.override { inherit (llvm) bintools; });

  stripped = kernel.override {
    ignoreConfigErrors = true;
    structuredExtraConfig = off strip;
  };

  lsmod = pkgs.writeText "lsmod" (
    "Module Size Used by\n" + lib.concatMapStringsSep "\n" (m: "${m} 0 0") modules
  );

  configfile = stdenv.mkDerivation {
    name = "linux-localmodconfig-${kernel.version}";
    inherit (kernel) src;
    nativeBuildInputs = kernel.nativeBuildInputs or [ ];
    buildInputs = kernel.buildInputs or [ ];
    dontConfigure = true;
    dontPatch = true;

    buildPhase = ''
      cp ${stripped.configfile} .config
      chmod +w .config
      set +o pipefail
      yes "" | make ARCH=x86_64 CC=clang HOSTCC=cc LSMOD=${lsmod} localmodconfig
      set -o pipefail
      patchShebangs scripts/config
      scripts/config --disable MODULE_COMPRESS_XZ --enable MODULE_COMPRESS_ZSTD
      scripts/config --module CRYPTO_LZ4HC --module CRYPTO_CRC32C

      # see `security.lsm`
      scripts/config \
        --disable SECURITY_SELINUX \
        --disable SECURITY_APPARMOR \
        --disable SECURITY_SMACK \
        --disable SECURITY_TOMOYO \
        --disable SECURITY_IPE \
        --disable SECURITY_LOADPIN \
        --disable SECURITY_SAFESETID \
        --disable SECURITY_LOCKDOWN_LSM \
        --disable IMA \
        --disable EVM \
        --set-str LSM "landlock,yama,bpf"

      make ARCH=x86_64 CC=clang HOSTCC=cc olddefconfig
    '';

    installPhase = "cp .config $out";
  };

  hyper-v = pkgs.linuxKernel.manualConfig {
    inherit (kernel) src version features;
    inherit configfile stdenv;
    modDirVersion = lib.versions.pad 3 kernel.version;
    allowImportFromDerivation = true;

    kernelPatches = [
      pkgs.linuxKernel.kernelPatches.bridge_stp_helper
      pkgs.linuxKernel.kernelPatches.request_key_helper
    ];

    extraMeta.branch = lib.versions.majorMinor kernel.version;
  };
in
{
  boot.kernelPackages = pkgs.linuxKernel.packagesFor hyper-v;
}
