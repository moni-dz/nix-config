_:

{
  flake.overlays.default = final: prev: {
    asitop = prev.callPackage ./derivations/asitop.nix { };

    fanbox-dl = prev.callPackage ./derivations/fanbox-dl.nix { };

    python3 = prev.python3.override {
      packageOverrides = pfinal: pprev: {
        pydashing = pfinal.callPackage ./derivations/dashing.nix { };
      };
    };

    firefox-unwrapped = prev.firefox-unwrapped.overrideAttrs (old: {
      patches = (old.patches or [ ]) ++ [ ./patches/D164578.diff ];
    });

    iosevka-ft = prev.iosevka.override {
      privateBuildPlan = {
        export-glyph-names = true;
        family = "Iosevka FT";
        ligations.inherits = "dlig";
        no-cv-ss = false;
        serifs = "sans";
        spacing = "term";

        variants = {
          inherits = "ss14";

          design = {
            ascii-grave = "raised-turn-comma";
            ascii-single-quote = "raised-comma";
            brace = "curly-flat-boundary";
            capital-j = "serifless";
            capital-k = "curly-serifless";
            capital-r = "curly";
            caret = "high";
            cyrl-capital-ka = "curly-serifless";
            cyrl-capital-u = "cursive";
            cyrl-ka = "curly-serifless";
            eight = "crossing-asymmetric";
            four = "closed";
            g = "single-storey-serifless";
            k = "curly-serifless";
            lower-alpha = "crossing";
            lower-lambda = "straight-turn";
            nine = "closed-contour";
            paren = "normal";
            q = "earless-corner";
            question = "smooth";
            seven = "straight-serifless";
            six = "closed-contour";
            t = "flat-hook-short-neck";
            three = "two-arcs";
            underscore = "above-baseline";
            y = "cursive";
          };

          italic = {
            a = "double-storey-serifless";
            f = "serifless";
          };

          oblique = {
            cyrl-ef = "cursive";
            f = "serifless";
          };
        };
      };

      set = "ft";
    };
  };
}
