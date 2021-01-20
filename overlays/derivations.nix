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
    fetchPypi = prev.python39Packages.fetchPypi;
  };
  eww = prev.callPackage ../derivations/eww.nix {
    rustPlatform = prev.rustPlatform;
    fetchFromGitHub = prev.fetchFromGitHub;
    rust = prev.latest.rustChannels.nightly.rust;
    gtk3 = prev.gtk3;
    cairo = prev.cairo;
    glib = prev.glib;
    pkg-config = prev.pkg-config;
  };
  phocus = prev.callPackage ../derivations/phocus.nix {
    sass = prev.sass;
    fetchFromGitHub = prev.fetchFromGitHub;
    stdenv = prev.stdenv;
  };
  zls = prev.callPackage ../derivations/zls.nix {
    zig = prev.zig;
    stdenv = prev.stdenv;
    fetchFromGithub = prev.fetchFromGitHub;
  };
  river = prev.callPackage ../derivations/river.nix {
    stdenv = prev.stdenv;
    pkgs = prev;
  };
  scroll =
    prev.callPackage ../derivations/scroll.nix { stdenv = prev.stdenv; };
}
