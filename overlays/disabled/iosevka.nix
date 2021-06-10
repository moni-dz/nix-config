final: prev: {
  iosevka-ft = prev.iosevka7.iosevka.override {
    privateBuildPlan = ''
      [buildPlans.iosevka-ft]
      family = "Iosevka FT"
      spacing = "fontconfig-mono"
      serifs = "sans"
      no-cv-ss = false
      export-glyph-names = true

        [buildPlans.iosevka-ft.variants]
        inherits = "ss14"

          [buildPlans.iosevka-ft.variants.design]
          capital-r = "curly"
          g = "earless-corner"
          k = "curly-serifless"
          q = "earless-corner"
          y = "cursive"
          lower-alpha = "crossing"
          lower-lambda = "straight-turn"
          cyrl-ka = "curly-serifless"
          cyrl-capital-u = "cursive"
          zero = "long-dotted"
          three = "twoarcs"
          four = "closed"
          six = "closed-contour"
          seven = "straight-serifless"
          eight = "crossing-asymmetric"
          nine = "closed-contour"
          underscore = "above-baseline"
          caret = "high"
          paren = "normal"
          at = "fourfold"
          ascii-single-quote = "raised-comma"
          ascii-grave = "raised-turn-comma"
          question = "smooth"

          [buildPlans.iosevka-ft.variants.italic]
          a = "double-storey-serifless"
          f = "serifless"

        [buildPlans.iosevka-ft.ligations]
        inherits = "dlig"
    '';

    set = "ft";
  };
}
