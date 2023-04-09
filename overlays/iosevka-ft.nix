final: prev: {
  iosevka-ft = prev.iosevka.override {
    privateBuildPlan = ''
      [buildPlans.iosevka-ft]
      family = "Iosevka FT"
      spacing = "term"
      serifs = "sans"
      no-cv-ss = false
      export-glyph-names = true

        [buildPlans.iosevka-ft.variants]
        inherits = "ss14"

          [buildPlans.iosevka-ft.variants.design]
          capital-j = "serifless"
          capital-k = "curly-serifless"
          capital-r = "curly"
          g = "single-storey-serifless"
          k = "curly-serifless"
          q = "earless-corner"
          t = "flat-hook-short-neck"
          y = "cursive"
          lower-alpha = "crossing"
          lower-lambda = "straight-turn"
          cyrl-capital-ka = "curly-serifless"
          cyrl-ka = "curly-serifless"
          cyrl-capital-u = "cursive"
          three = "two-arcs"
          four = "closed"
          six = "closed-contour"
          seven = "straight-serifless"
          eight = "crossing-asymmetric"
          nine = "closed-contour"
          underscore = "above-baseline"
          caret = "high"
          paren = "normal"
          brace = "curly-flat-boundary"
          ascii-single-quote = "raised-comma"
          ascii-grave = "raised-turn-comma"
          question = "smooth"

          [buildPlans.iosevka-ft.variants.italic]
          a = "double-storey-serifless"
          f = "serifless"

         [buildPlans.iosevka-ft.variants.oblique]
         f = "serifless"
         cyrl-ef = "cursive"

      [buildPlans.iosevka-ft.ligations]
      inherits = "dlig"
    '';

    set = "ft";
  };
}
