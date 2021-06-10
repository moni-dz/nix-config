final: prev: {
  picom = prev.picom.overrideAttrs (old: { src = prev.picom-src; });
}
