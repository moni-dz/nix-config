{ pkgs }:

[
  rec {
    name = "fast-syntax-highlighting";
    src = pkgs.fetchFromGitHub {
      owner = "zdharma";
      repo = name;
      rev = "a62d721affc771de2c78201d868d80668a84c1e1";
      sha256 = "sha256-4xJXH9Wn18/+Vfib/ZrhCRp/yB1PppsbZCx1/WafmU8=";
    };
  }

  {
    name = "doas";
    src = pkgs.fetchFromGitHub {
      owner = "anatolykopyl";
      repo = "doas-zsh-plugin";
      rev = "17d0b55ca2acd12f7acc9e38c4ecaf413725be18";
      sha256 = "sha256-10rcF9cho9GuZCFQVIdFjvHCAlTLHNaLY4twxjT2jcE=";
    };
  }
]
