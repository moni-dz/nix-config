{ colorscheme }:

with colorscheme.colors;

''
  :root {
    --font-primary: "Sarasa Gothic J" !important;
    --font-display: "Sarasa Gothic J" !important;
    --font-code: "Iosevka FT Light" !important;
    --font-headline: "Sarasa Gothic J" !important;
    --font-korean: "Sarasa Term K Light" !important;
    --font-japanese: "Sarasa Term J Light" !important;
    --font-chinese-simplified: "Sarasa Term C Light" !important;
    --font-chinese-traditional: "Sarasa Term C Light" !important;
  }

  div[id ^= "uid_"], .content-1LAB8Z {
    background-color: var(--background-secondary) !important;
  }

  #online-tab {
    background-color: var(--background-primary) !important;
  }

  .notches-1sAcEM.gray-3_LNYR {
    background-color: var(--background-primary) !important;
  }

  .mention {
    background-color: #${base0D}4d !important;
  }

  span[class *= "botTag"] {
    background-color: #${base0D} !important;
    color: #${base01} !important;
  }

  pre,
  code,
  .codeBlockText-9TnoxQ,
  [class *= "codeLine"],
  .syntaxBefore-1YQ9lW.before_inlineCode-1G9rTK,
  [class *= "inlineCode"] > span,
  .syntaxAfter-hcA4AH.after_inlineCode-1KfVgj,
  [class *= "hljs"],
  .markup-2BOw-j.slateTextArea-1Mkdgw.fontSize16Padding-3Wk7zP.textAreaWithoutAttachmentButton-qiaiTB,
  .markup-2BOw-j.slateTextArea-1Mkdgw.fontSize16Padding-3Wk7zP.textAreaWithoutAttachmentButton-qiaiTB > div > span {
    font-family: var(--font-code) !important;
    color: var(--text-normal);
    font-size: 14.5px !important;
  }

  a[href ^= "http:"], a[href ^= "https:"] {
    color: var(--text-link) !important;
  }

  .hljs-class, .hljs-function, .hljs-keyword {
    color: #${base0B} !important;
  }

  .hljs-built_in {
    color: #${base08} !important;
  }

  .hljs-title, .hljs-name {
    color: #${base0D} !important;
  }

  .hljs-string {
    color: #${base0A} !important;
  }

  .hljs-number {
    color: #${base09} !important;
  }

  .hljs-symbol {
    color: #${base0F} !important;
  }

  .hljs-meta, .hljs-meta-keyword {
    color: #${base0E} !important;
  }

  .hljs-meta-string {
    color: #${base0A} !important;
  }

  .hljs-comment, .hljs-quote {
    color: var(--text-muted) !important;
  }

  .hljs-params {
    color: var(--text-normal) !important;
  }

  .barButtonMain-3K-jeJ.barButtonBase-2uLO1z,
  .barButtonAlt-mYL1lj.barButtonBase-2uLO1z {
    background-color: #${base0B} !important;
    color: #${base02} !important;
  }

  .theme-dark {
    --header-primary: #${base05} !important;
    --header-secondary: #${base04} !important;
    --text-normal: #${base05} !important;
    --text-muted: #${base04} !important;
    --text-link: #${base0B} !important;
    --text-link-low-saturation: #${base0B} !important;
    --text-positive: hsl(139,calc(var(--saturation-factor, 1)*66.8%),58.6%);
    --text-warning: hsl(38,calc(var(--saturation-factor, 1)*95.7%),54.1%);
    --text-danger: hsl(359,calc(var(--saturation-factor, 1)*82.6%),59.4%);
    --text-brand: hsl(235,calc(var(--saturation-factor, 1)*86.1%),77.5%);
    --interactive-normal: #b9bbbe;
    --interactive-hover: #dcddde;
    --interactive-active: #fff;
    --interactive-muted: #4f545c;
    --background-primary: #${base00} !important;
    --background-secondary: #${base01} !important;
    --background-secondary-alt: #${base01} !important;
    --background-tertiary: #${base02} !important;
    --background-accent: #4f545c;
    --background-floating: #18191c;
    --background-nested-floating: #2f3136;
    --background-mobile-primary: #36393f;
    --background-mobile-secondary: #2f3136;
    --background-modifier-hover: rgba(79,84,92,0.16);
    --background-modifier-active: rgba(79,84,92,0.24);
    --background-modifier-selected: rgba(79,84,92,0.32);
    --background-modifier-accent: hsla(0,0%,100%,0.06);
    --brand-experiment: #${base0D} !important;
    --brand-experiment-05a: #${base0D}5a !important;
    --info-positive-text: #fff;
    --info-warning-text: #fff;
    --info-warning-foreground: #${base08} !important;
    --info-danger-text: #fff;
    --info-help-background: hsla(197,calc(var(--saturation-factor, 1)*100%),47.8%,0.1);
    --info-help-foreground: hsl(197,calc(var(--saturation-factor, 1)*100%),47.8%);
    --info-help-text: #fff;
    --status-warning-text: #000;
    --scrollbar-thin-thumb: #${base00} !important;
    --scrollbar-thin-track: transparent;
    --scrollbar-auto-thumb: #${base04} !important;
    --scrollbar-auto-track: hsl(210,calc(var(--saturation-factor, 1)*9.8%),20%);
    --scrollbar-auto-scrollbar-color-thumb: #${base04} !important;
    --scrollbar-auto-scrollbar-color-track: #${base00} !important;
    --elevation-stroke: 0 0 0 1px rgba(4,4,5,0.15);
    --elevation-low: 0 1px 0 rgba(4,4,5,0.2),0 1.5px 0 rgba(6,6,7,0.05),0 2px 0 rgba(4,4,5,0.05);
    --elevation-medium: 0 4px 4px rgba(0,0,0,0.16);
    --elevation-high: 0 8px 16px rgba(0,0,0,0.24);
    --logo-primary: #fff;
    --control-brand-foreground: hsl(235,calc(var(--saturation-factor, 1)*86.1%),77.5%);
    --control-brand-foreground-new: hsl(235,calc(var(--saturation-factor, 1)*86.1%),77.5%);
    --background-mentioned: #${base08}3d;
    --background-mentioned-hover: #${base08}2d;
    --background-message-hover: rgba(4,4,5,0.07);
    --channels-default: #8e9297;
    --guild-header-text-shadow: 0 1px 1px rgba(0,0,0,0.4);
    --channeltextarea-background: #${base02} !important;
    --activity-card-background: #202225;
    --textbox-markdown-syntax: #8e9297;
    --deprecated-card-bg: rgba(32,34,37,0.6);
    --deprecated-card-editable-bg: rgba(32,34,37,0.3);
    --deprecated-store-bg: #36393f;
    --deprecated-quickswitcher-input-background: #72767d;
    --deprecated-quickswitcher-input-placeholder: hsla(0,0%,100%,0.3);
    --deprecated-text-input-bg: rgba(0,0,0,0.1);
    --deprecated-text-input-border: rgba(0,0,0,0.3);
    --deprecated-text-input-border-hover: #040405;
    --deprecated-text-input-border-disabled: #202225;
    --deprecated-text-input-prefix: #dcddde;
  }
''
