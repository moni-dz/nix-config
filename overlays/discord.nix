final: prev: {
  discord-openasar = prev.discord-openasar.overrideAttrs (old: {
    __contentAddressed = true;

    postFixup = (old.postFixup or "") + ''
      wrapProgram $out/bin/discord \
        --add-flags "--enable-features=UseOzonePlatform" \
        --add-flags "--ozone-platform=wayland"
    '';
  });
}