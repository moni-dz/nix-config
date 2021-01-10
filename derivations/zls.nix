{ stdenv, lib, fetchFromGitHub, zig }:

stdenv.mkDerivation rec {
  pname = "zls";
  version = "master";

  src = fetchFromGitHub {
    owner = "zigtools";
    repo = pname;
    rev = version;
    sha256 = lib.fakeSha256;
    fetchSubmodules = true;
  };

  buildInputs = [ zig ];

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
