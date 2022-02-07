final: prev: {
  steam = prev.steam.override {
    extraPkgs = pkgs: with pkgs; [
      glfw-minecraft-wayland
    ];
  };
}
