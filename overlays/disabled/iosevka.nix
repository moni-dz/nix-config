final: prev: {
  iosevka-ft = prev.iosevka-6.iosevka.override {
    privateBuildPlan = ''
      [buildPlans.iosevka-ft]
      family = "Iosevka FT"
      spacing = "fontconfig-mono"
      serifs = "sans"
      no-cv-ss = false

        [buildPlans.iosevka-ft.variants]
        inherits = "ss14"

          [buildPlans.iosevka-ft.variants.design]
          capital-q = "straight"
          capital-r = "curly"
          capital-w = "straight-flat-top"
          capital-y = "straight-serifless"
          g = "single-storey-earless-corner-flat-hook"
          i = "serifed"
          j = "flat-hook-serifed"
          k = "curly"
          m = "normal"
          n = "straight"
          q = "earless-corner"
          r = "serifless"
          w = "straight-flat-top"
          y = "cursive-flat-hook"
          lower-alpha = "crossing"
          lower-lambda = "straight-turn"
          cyrl-ka = "straight"
          cyrl-capital-u = "cursive-flat-hook"
          zero = "long-dotted"
          one = "base"
          two = "straight-neck"
          three = "twoarcs"
          four = "closed"
          five = "oblique-upper-left-bar"
          six = "closed-contour"
          seven = "straight-serifless"
          eight = "two-circles"
          nine = "closed-contour"
          asterisk = "hex-low"
          underscore = "above-baseline"
          caret = "high"
          paren = "large-contour"
          number-sign = "slanted-open"
          ampersand = "upper-open"
          at = "threefold"
          dollar = "through"
          cent = "through"
          lig-ltgteq = "slanted"
          ascii-single-quote = "raised-comma"
          ascii-grave = "raised-turn-comma"
          question = "smooth"

          [buildPlans.iosevka-ft.variants.italic]
          a = "double-storey"

        [buildPlans.iosevka-ft.ligations]
        inherits = "dlig"
    '';

    set = "ft";
  };
}
