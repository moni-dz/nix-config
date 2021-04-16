final: prev: {
  slock = prev.slock.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "khuedoan";
      repo = "slock";
      rev = "37f091cb167f719103ef70baa6b46b95645e5b95";
      sha256 = "sha256-++R5tTk4nbNo3elzoD2jJbEBAV7gjxUf8Cegeo0fVGA=";
    };

    patches = [ ./patches/slock-patch.diff ];
  });
}
