final: prev: {
  iosevka-ft = prev.head.iosevka.override {
    privateBuildPlan = ''
      [buildPlans.iosevka-ft]
      family = "Iosevka FT"
      spacing = "fontconfig-mono"
      serifs = "sans"

      [buildPlans.iosevka-ft.variants]
      inherits = "ss14"

      [buildPlans.iosevka-ft.variants.design]
      g = "single-storey-earless-corner-flat-hook"
      t = "flat-hook"
      y = "cursive"
      lower-alpha = "crossing"
      zero = "long-dotted"
      five = "oblique-upper-left-bar"
      six = "closed-contour"
      seven = "straight-serifless"
      eight = "two-circles"
      nine = "closed-contour"
      underscore = "above-baseline"
      caret = "high"
      paren = "large-contour"
      number-sign = "slanted-open"
      ascii-single-quote = "raised-comma"
      ascii-grave = "raised-turn-comma"
      question = "smooth"

      [buildPlans.iosevka-ft.variants.italic]
      a = "double-storey-tailed"

      [buildPlans.iosevka-ft.variants.oblique]
      a = "double-storey-tailed"

      [buildPlans.iosevka-ft.ligations]
      inherits = "dlig"
    '';
    set = "ft";
  };
}
