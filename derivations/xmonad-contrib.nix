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
