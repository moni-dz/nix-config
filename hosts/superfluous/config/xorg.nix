# NOTE: this is extra stuff that goes in xorg.conf

''
  Section "Device"
    Identifier "Radeon"
    Driver "radeon"
    Option "TearFree" "on"
  EndSection
  Section "Device"
    Identifier "AMD"
    Driver "amdgpu"
    Option "TearFree" "true"
  EndSection
''
