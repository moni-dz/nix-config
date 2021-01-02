final: prev: {
  weechat-unwrapped = prev.weechat-unwrapped.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "weechat";
      repo = "weechat";
      rev = "master";
      sha256 = "sha256-VfiRXHq7U5H2M/2US+B/iYbCNS7IuKGCoHD2fnYQVhE=";
    };
  });
}
