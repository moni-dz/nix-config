{ mkDerivation
, base
, containers
, data-default
, directory
, fetchgit
, filepath
, lib
, mtl
, process
, QuickCheck
, quickcheck-classes
, setlocale
, transformers
, unix
, utf8-string
, X11
, src
}:

mkDerivation {
  pname = "xmonad";
  version = "0.16.9999";

  inherit src;

  patches = [ ./patches/xmonad-nix.patch ];

  isLibrary = true;
  isExecutable = true;

  libraryHaskellDepends = [
    base
    containers
    data-default
    directory
    filepath
    mtl
    process
    setlocale
    transformers
    unix
    utf8-string
    X11
  ];

  executableHaskellDepends = [ base mtl unix X11 ];

  testHaskellDepends = [
    base
    containers
    QuickCheck
    quickcheck-classes
    X11
  ];

  postInstall = ''
    install -D man/xmonad.1 ''${!outputDoc}/share/man/man1/xmonad.1
    install -D man/xmonad.hs ''${!outputDoc}/share/doc/$name/sample-xmonad.hs
  '';

  homepage = "http://xmonad.org";
  description = "A tiling window manager";
  license = lib.licenses.bsd3;
}
