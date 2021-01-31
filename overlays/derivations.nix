final: prev: {
  bs4 = prev.callPackage ../derivations/bs4.nix {
    python38Packages = prev.python38Packages;
    buildPythonPackage = prev.python38Packages.buildPythonPackage;
    fetchPypi = prev.python39Packages.fetchPypi;
  };
  simber = prev.callPackage ../derivations/simber.nix {
    python38Packages = prev.python38Packages;
    buildPythonPackage = prev.python38Packages.buildPythonPackage;
    fetchPypi = prev.python39Packages.fetchPypi;
  };
  pydes = prev.callPackage ../derivations/pydes.nix {
    python38Packages = prev.python38Packages;
    buildPythonPackage = prev.python38Packages.buildPythonPackage;
    fetchPypi = prev.python39Packages.fetchPypi;
  };
  downloader-cli = prev.callPackage ../derivations/downloader-cli.nix {
    python38Packages = prev.python38Packages;
    buildPythonPackage = prev.python38Packages.buildPythonPackage;
    fetchFromGitHub = prev.fetchFromGitHub;
  };
  itunespy = prev.callPackage ../derivations/itunespy.nix {
    python38Packages = prev.python38Packages;
    buildPythonPackage = prev.python38Packages.buildPythonPackage;
    fetchPypi = prev.python39Packages.fetchPypi;
  };
  youtube-search = prev.callPackage ../derivations/youtube-search.nix {
    python38Packages = prev.python38Packages;
    buildPythonPackage = prev.python38Packages.buildPythonPackage;
    fetchPypi = prev.python39Packages.fetchPypi;
  };
  ytmdl = prev.callPackage ../derivations/ytmdl.nix {
    bs4 = final.bs4;
    pydes = final.pydes;
    simber = final.simber;
    itunespy = final.itunespy;
    downloader-cli = final.downloader-cli;
    youtube-search = final.youtube-search;
    python38Packages = prev.python38Packages;
    buildPythonPackage = prev.python38Packages.buildPythonPackage;
    fetchPypi = prev.python38Packages.fetchPypi;
  };
  wdpass = prev.callPackage ../derivations/wdpass.nix {
    python38Packages = prev.python38Packages;
    buildPythonPackage = prev.python38Packages.buildPythonPackage;
    fetchPypi = prev.python38Packages.fetchPypi;
  };
  eww = prev.callPackage ../derivations/eww.nix {
    rustPlatform = prev.rustPlatform;
    fetchFromGitHub = prev.fetchFromGitHub;
    rust = prev.rust-bin.nightly."2021-01-19".rust;
    pkgs = prev;
  };
  phocus = prev.callPackage ../derivations/phocus.nix {
    sass = prev.sass;
    fetchFromGitHub = prev.fetchFromGitHub;
    stdenv = prev.stdenv;
  };
  zls = prev.callPackage ../derivations/zls.nix {
    zig = prev.zig;
    stdenv = prev.stdenv;
    fetchgit = prev.fetchgit;
  };
  scientifica = prev.callPackage ../derivations/scientifica.nix {
    stdenv = prev.stdenv;
    fetchzip = prev.fetchzip;
  };
  river = prev.callPackage ../derivations/river.nix {
    stdenv = prev.stdenv;
    pkgs = prev;
  };
  output-fonts = prev.callPackage ../derivations/output-fonts.nix {
    stdenv = prev.stdenv;
    unzip = prev.unzip;
    requireFile = prev.requireFile;
  };
  sacad = prev.callPackage ../derivations/sacad.nix {
    python38Packages = prev.python38Packages;
    fetchPypi = prev.python38Packages.fetchPypi;
    web_cache = final.web_cache;
  };
  web_cache = prev.callPackage ../derivations/web-cache.nix {
    python38Packages = prev.python38Packages;
    fetchPypi = prev.python38Packages.fetchPypi;
  };
  pixcat = prev.callPackage ../derivations/pixcat.nix {
    python38Packages = prev.python38Packages;
    buildPythonPackage = prev.python38Packages.buildPythonPackage;
    fetchPypi = prev.python38Packages.fetchPypi;
  };
  miniplayer = prev.callPackage ../derivations/miniplayer.nix {
    python38Packages = prev.python38Packages;
    buildPythonPackage = prev.python38Packages.buildPythonPackage;
    fetchPypi = prev.python38Packages.fetchPypi;
    pixcat = final.pixcat;
  };
  scroll =
    prev.callPackage ../derivations/scroll.nix { stdenv = prev.stdenv; };
}
