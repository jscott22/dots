;;; doom-wal-theme.el
(require 'doom-themes)

;;
(defgroup doom-wal-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-wal-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-wal-theme
  :type 'boolean)

(defcustom doom-wal-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-wal-theme
  :type 'boolean)

(defcustom doom-wal-comment-bg doom-wal-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-wal-theme
  :type 'boolean)

(defcustom doom-wal-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-wal-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-wal
  "Doom wal theme"

  ;; name        default   256       16
  ((bg         '("#201121" nil       nil            ))
   (bg-alt     '("#201121" nil       nil            ))
   (base0      '("#141314" "#121212" "black"        ))
   (base1      '("#292629" "#262626" "brightblack"  ))
   (base2      '("#3D393D" "#3A3A3A" "brightblack"  ))
   (base3      '("#524C52" "#4E4E4E" "brightblack"  ))
   (base4      '("#665F66" "#626262" "brightblack"  ))
   (base5      '("#7A727A" "#767676" "brightblack"  ))
   (base6      '("#8E868E" "#8A8A8A" "brightblack"  ))
   (base7      '("#A19AA1" "#9E9E9E" "brightblack"  ))
   (base8      '("#B4AFB4" "#B2B2B2" "white"        ))
   (fg-alt     '("#D2CFD2" "#D0D0D0" "brightwhite"  ))
   (fg         '("#C7C3C7" "#C6C6C6" "white"        ))

   (grey       base4)
   (red        '("#B74FBE" "#AF5FAF" "red"          ))
   (orange     '("#C356CF" "#AF5FD7" "brightred"    ))
   (green      '("#466CA1" "#5F5FAF" "green"        ))
   (teal       '("#6488BC" "#5F87AF" "brightgreen"  ))
   (yellow     '("#CF5DE0" "#D75FD7" "yellow"       ))
   (blue       '("#8A7FE7" "#8787D7" "brightblue"   ))
   (dark-blue  '("#5243DC" "#5F5FD7" "blue"         ))
   (magenta    '("#5880AE" "#5F87AF" "magenta"      ))
   (violet     '("#84A1C3" "#87AFAF" "brightmagenta"))
   (cyan       '("#96B3D5" "#87AFD7" "brightcyan"   ))
   (dark-cyan  '("#628EC0" "#5F87AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-lighten bg 0.05))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-wal-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-wal-brighter-comments dark-cyan base5) 0.25))
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
   (-modeline-bright doom-wal-brighter-modeline)
   (-modeline-pad
    (when doom-wal-padded-modeline
      (if (integerp doom-wal-padded-modeline) doom-wal-padded-modeline 4)))

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
    :background (if doom-wal-comment-bg (doom-lighten bg 0.05)))
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

;;; doom-wal-theme.el ends here
