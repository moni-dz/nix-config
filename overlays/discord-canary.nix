final: prev: {
  discord-canary = prev.discord-canary.overrideAttrs (old: rec {
    powercord = prev.stdenv.mkDerivation rec {
      pname = "powercord";
      version = "v2";

      src = prev.fetchFromGitHub {
        owner = "powercord-org";
        repo = pname;
        rev = version;
        sha256 = "sha256-kFf3U7BJAOrRTHemxAqVfFnLxXZqMQYd+IRxPoVNgvQ=";
      };

      nativeBuildInputs = with prev; [ discord-canary nodePackages.npm ];

      buildPhase =
        "${prev.nodePackages.npm}/bin/npm i"; # FIXME: do I disable nix.useSandbox?

      installPhase =
        "substituteInPlace injectors/linux.js --replace /opt/DiscordCanary ${prev.discord-canary}/opt/DiscordCanary";

      meta = with prev.stdenv.lib; {
        description =
          "A lightweight Discord client mod focused on simplicity and performance";
        homepage = "https://powercord.dev";
        licenses = licenses.mit;
        maintainers = with maintainers; [ fortuneteller2k ];
      };
    };

    buildInputs = (old.buildInputs or [ ]) ++ [ prev.nodePackages.npm ];
    installPhase = old.installPhase + ''
      cd ${powercord}
      ${prev.nodePackages.npm}/bin/npm run plug
    '';
  });
}
