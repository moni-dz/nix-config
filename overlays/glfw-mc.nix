final: prev: {
  glfw-minecraft-wayland = prev.glfw-wayland.overrideAttrs (old: {
    patches = [
      # Wayland: Set O_NONBLOCK on repeat timerfd
      (prev.fetchpatch {
        url = "https://raw.githubusercontent.com/Admicos/minecraft-wayland/main/0001-Wayland-Set-O_NONBLOCK-on-repeat-timerfd.patch";
        hash = "sha256-CdysDWCuNBwn3mtOyy+z4ssZc8gW7rLxQ33sJBnpCCo=";
      })

      # Wayland: Continue poll() if timerfd can't be read
      (prev.fetchpatch {
        url = "https://raw.githubusercontent.com/Admicos/minecraft-wayland/main/0002-Wayland-Continue-poll-if-timerfd-can-t-be-read.patch";
        hash = "sha256-yka4bGXAI+waGPztti6fic/uCrlw7k0Lhcx9DHnwtvg=";
      })

      # Don't crash on calls to focus or icon
      (prev.fetchpatch {
        url = "https://raw.githubusercontent.com/Admicos/minecraft-wayland/main/0003-Don-t-crash-on-calls-to-focus-or-icon.patch";
        hash = "sha256-3U/nzFUI8nz3ixxhRFzgppoWH62kNMlGJnXSaJPbtRY=";
      })

      # wayland: fix broken opengl screenshots on mutter
      (prev.fetchpatch {
        url = "https://raw.githubusercontent.com/Admicos/minecraft-wayland/main/0004-wayland-fix-broken-opengl-screenshots-on-mutter.patch";
        hash = "sha256-ZVlnXZkqp7B5WZzzkMGjAyYvjmidlZyYvpa0z3GNW4U=";
      })

      # Add warning about being an unofficial patch
      (prev.fetchpatch {
        url = "https://raw.githubusercontent.com/Admicos/minecraft-wayland/main/0005-Add-warning-about-being-an-unofficial-patch.patch";
        hash = "sha256-j/z6c/bGKFtCwBvIVNGi63xa+7yIF1mRKc9q3Ykigaw=";
      })
    ];
  });
}
