{ pkgs, lib, stdenv, fetchFromGitHub, ... }:

let
  themeName = "f2k-phocus";
in stdenv.mkDerivation rec {
  src = fetchFromGitHub {
    owner = "fortuneteller2k";
    repo = "gtk";
    rev = "master";
    sha256 = "";
  };

  nativeBuildInputs = with pkgs; [
    sass
  ];

  installFlags = [
    "DESTDIR=$(out)"
    "PREFIX="
  ];

  meta = with lib; {
    inherit version;
    description = "Fork of Elkowar's Phocus fork.";
    homepage = "https://github.com/fortuneteller2k/phocus";
    license = licenses.mit;
    maintainers = with maintainers; [ fortuneteller2k ];
  };
}
