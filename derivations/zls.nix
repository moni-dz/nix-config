{ stdenv, lib, fetchgit, zig }:

stdenv.mkDerivation rec {
  pname = "zls";
  version = "0.1.0";

  src = fetchgit {
    url = "https://github.com/zigtools/zls"; 
    sha256 = "sha256-tLGsSvfp780cNgTME7IgfRfPRbjNvYKCwoNYiAKewSI=";
    fetchSubmodules = true;
  };

  buildInputs = [ zig ];

  preBuild = "export HOME=$TMPDIR;";
  installPhase = ''
    zig build -Drelease-safe
    zig build config
  '';

  meta = with lib; {
    description = "Zig Language Server, or zls, is a language server for Zig";
    homepage = "https://github.com/zigtools/zls";
    license = [ licenses.mit ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
