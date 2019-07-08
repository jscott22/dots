;;; doom-foo-theme.el
(require 'doom-themes)

;;
(defgroup doom-foo-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-foo-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-foo-theme
  :type 'boolean)

(defcustom doom-foo-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-foo-theme
  :type 'boolean)

(defcustom doom-foo-comment-bg doom-foo-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-foo-theme
  :type 'boolean)

(defcustom doom-foo-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-foo-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-foo
  "Doom foo theme"

  ;; name        default   256       16
  ((bg         '("#04020B" nil       nil            ))
   (bg-alt     '("#130A35" nil       nil            ))
   (base0      '("#0D1418" "#121212" "black"        ))
   (base1      '("#1A2731" "#262626" "brightblack"  ))
   (base2      '("#263B49" "#3A3A3A" "brightblack"  ))
   (base3      '("#334E61" "#4E4E4E" "brightblack"  ))
   (base4      '("#406279" "#5F5F87" "brightblack"  ))
   (base5      '("#4D7592" "#5F8787" "brightblack"  ))
   (base6      '("#5B88A9" "#5F87AF" "brightblack"  ))
   (base7      '("#739AB5" "#8787AF" "brightblack"  ))
   (base8      '("#8CABC2" "#87AFAF" "white"        ))
   (fg-alt     '("#B6CAD9" "#AFD7D7" "brightwhite"  ))
   (fg         '("#A4BDCF" "#AFAFD7" "white"        ))

   (grey       base4)
   (red        '("#2E3568" "#444444" "red"          ))
   (orange     '("#5F3471" "#585858" "brightred"    ))
   (green      '("#392A55" "#3A3A3A" "green"        ))
   (teal       '("#47346A" "#4E4E4E" "brightgreen"  ))
   (yellow     '("#90327A" "#875F87" "yellow"       ))
   (blue       '("#384F87" "#5F5F87" "brightblue"   ))
   (dark-blue  '("#2D3F6C" "#444444" "blue"         ))
   (magenta    '("#3E5379" "#5F5F87" "magenta"      ))
   (violet     '("#4E6897" "#5F5F87" "brightmagenta"))
   (cyan       '("#C228B6" "#AF00AF" "brightcyan"   ))
   (dark-cyan  '("#9B2091" "#AF0087" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-lighten bg 0.05))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-foo-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-foo-brighter-comments dark-cyan base5) 0.25))
   (constants      red)
   (functions      yellow)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        teal)
   (variables      cyan)
   (numbers        magenta)
   (region         dark-blue)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-foo-brighter-modeline)
   (-modeline-pad
    (when doom-foo-padded-modeline
      (if (integerp doom-foo-padded-modeline) doom-foo-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground fg-alt)
   ((line-number-current-line &override) :foreground fg)
   ((line-number &override) :background (doom-darken bg 0.025))

   (font-lock-comment-face
    :foreground comments
    :background (if doom-foo-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))
   (mode-line-buffer-id
    :foreground highlight)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (telephone-line-accent-active
    :inherit 'mode-line
    :background (doom-lighten bg 0.2))
   (telephone-line-accent-inactive
    :inherit 'mode-line
    :background (doom-lighten bg 0.05))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (org-block :background base2)
   (org-block-begin-line :background base2 :foreground comments)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-foo-theme.el ends here