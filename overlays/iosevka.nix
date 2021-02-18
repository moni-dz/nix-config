final: prev: {
  iosevka-ft = prev.master.iosevka.override {
    privateBuildPlan = ''
      [buildPlans.iosevka-ft]
      family = "Iosevka FT"
      spacing = "fontconfig-mono"
      serifs = "sans"

        [buildPlans.iosevka-ft.variants]
        inherits = "ss02"

          [buildPlans.iosevka-ft.variants.design]
          latn-epsilon = "serifed"
          lower-iota = "serifed-tailed"
          lower-lambda = "curly-turn"
          cyrl-ka = "curly-motion-serifed"
          zero = "reverse-slashed"
          tilde = "low"
          asterisk = "hex-low"
          underscore = "above-baseline"
          paragraph-sign = "low"
          caret = "low"
          paren = "large-contour"
          brace = "curly"
          number-sign = "slanted"
          ampersand = "upper-open"
          at = "short"
          dollar = "interrupted"
          bar = "natural-slope"
          lig-ltgteq = "slanted"
          ascii-single-quote = "straight"
          ascii-grave = "straight"

        [buildPlans.iosevka-ft.ligations]
        inherits = "dlig"
    '';
    set = "ft";
  };
}
