final: prev: {
  hilbish = prev.callPackage "${prev.path}/pkgs/shells/hilbish" {
    buildGoModule = args: prev.buildGoModule (args // {
      version = "2.0.1";

      src = prev.fetchFromGitHub {
        owner = "Rosettea";
        repo = "Hilbish";
        rev = "v2.0.1";
        hash = "sha256-5GPJVusF3/WbEE5VH45Wyxq40wbNxgjO8QI2cLMpdN0=";
        fetchSubmodules = true;
      };

      vendorSha256 = "sha256-Kiy1JR3X++naY2XNLpnGujrNQt7qlL0zxv8E96cHmHo=";

      postInstall = ''
        mkdir -p "$out/share/hilbish"

        cp .hilbishrc.lua $out/share/hilbish/
        cp -r docs -t $out/share/hilbish/
        cp -r libs -t $out/share/hilbish/
        cp -r nature -t $out/share/hilbish/
      '';
    });
  };
}
