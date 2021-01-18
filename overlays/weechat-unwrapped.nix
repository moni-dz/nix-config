final: prev: {
  weechat-unwrapped = prev.weechat-unwrapped.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "weechat";
      repo = "weechat";
      rev = "master";
      sha256 = "sha256-BWjDVC2ekwvbplYYZVL4NtmK7cELNjvYxsatekUT5Jc=";
    };
  });
}
