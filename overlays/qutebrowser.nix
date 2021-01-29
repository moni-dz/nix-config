final: prev: {
  qutebrowser = prev.qutebrowser.overrideAttrs (old: rec {
    version = "2.0.1";

    src = prev.fetchurl {
      url = "https://github.com/qutebrowser/qutebrowser/releases/download/v${version}/${old.pname}-${version}.tar.gz";
      sha256 = "sha256-qpiNYfGjltfnEBBUhRKRz1fnzxnn/A7v/TC2clcKskI=";
    };

    propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ [
      prev.python38Packages.importlib-resources
      prev.python38Packages.adblock
      prev.python38Packages.colorama
    ];
  });
}
