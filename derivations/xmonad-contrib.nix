{ mkDerivation
, base
, bytestring
, containers
, directory
, fetchgit
, filepath
, hspec
, lib
, mtl
, process
, QuickCheck
, random
, time
, unix
, utf8-string
, X11
, X11-xft
, xmonad
, src
}:

mkDerivation {
  pname = "xmonad-contrib";
  version = "0.16.999";
  #src = fetchgit {
  #  url = "https://github.com/xmonad/xmonad-contrib";
  #  sha256 = "0xygvq9sbal6aqb3mk575zz04lilp9wlbilj8fbpni8jmq082mvk";
  #  rev = "402d29b306950b9f74664b7652d2b9f3bfa40895";
  #  fetchSubmodules = true;
  #};
  inherit src;
  libraryHaskellDepends = [
    base
    bytestring
    containers
    directory
    filepath
    mtl
    process
    random
    time
    unix
    utf8-string
    X11
    X11-xft
    xmonad
  ];
  testHaskellDepends = [
    base
    containers
    directory
    hspec
    mtl
    process
    QuickCheck
    unix
    utf8-string
    X11
    xmonad
  ];
  homepage = "http://xmonad.org/";
  description = "Third party extensions for xmonad";
  license = lib.licenses.bsd3;
}
