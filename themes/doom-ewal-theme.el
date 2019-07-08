;;; doom-ewal-theme.el --- inspired by Atom One Dark
(require 'doom-themes)
(require 'ewal)
(ewal-load-ewal-colors)
(defgroup doom-ewal-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-ewal-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-ewal-theme
  :type 'boolean)

(defcustom doom-ewal-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ewal-theme
  :type 'boolean)

(defcustom doom-ewal-comment-bg doom-ewal-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-ewal-theme
  :type 'boolean)

(defcustom doom-ewal-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-ewal-theme
  :type '(or integer boolean))

;; ((comment "#444f56" "#4a555d" "#4f5c64" "#55636c" "#5b6973" "#60707a" "#667681" "#6c7d88" "#728490" "#798a95" "#80909b" "#8796a0" "#8e9ca6" "#95a2ab" "#9ca8b1" "#a3afb6" "#aab5bc")
;;  (background "#020106" "#020107" "#020107" "#030108" "#030108" "#030109" "#030109" "#03010a" "#04020B" "#100e17" "#1d1b23" "#29272f" "#36343b" "#424148" "#4f4d54" "#5b5a60" "#68676c")
;;  (foreground "#62717c" "#6a7a86" "#728490" "#7b8d9b" "#8397a5" "#8ba0af" "#93aaba" "#9bb3c4" "#a4bdcf" "#a8c0d1" "#adc3d3" "#b1c6d6" "#b6cad8" "#bacddb" "#bfd0dd" "#c3d4df" "#c8d7e2")
;;  (cursor "#62717c" "#6a7a86" "#728490" "#7b8d9b" "#8397a5" "#8ba0af" "#93aaba" "#9bb3c4" "#a4bdcf" "#a8c0d1" "#adc3d3" "#b1c6d6" "#b6cad8" "#bacddb" "#bfd0dd" "#c3d4df" "#c8d7e2")
;;  (black "#020106" "#020107" "#020107" "#030108" "#030108" "#030109" "#030109" "#03010a" "#04020B" "#100e17" "#1d1b23" "#29272f" "#36343b" "#424148" "#4f4d54" "#5b5a60" "#68676c")
;;  (red "#1b1f3e" "#1d2243" "#202548" "#22274e" "#242a53" "#272d58" "#292f5d" "#2b3262" "#2E3568" "#383f6f" "#424977" "#4d537e" "#575d86" "#62678d" "#6c7195" "#777b9c" "#8185a4")
;;  (green "#2a1f3f" "#2e2144" "#31244a" "#35274f" "#382954" "#3c2c5a" "#3f2e5f" "#433164" "#47346A" "#503e71" "#594878" "#625280" "#6b5c87" "#75668f" "#7e7096" "#877b9e" "#9085a5")
;;  (yellow "#561e49" "#5d204f" "#642255" "#6c255b" "#732861" "#7a2a67" "#812d6d" "#882f73" "#90327A" "#953c80" "#9b4687" "#a0508d" "#a65a94" "#ab659b" "#b16fa1" "#b679a8" "#bc84af")
;;  (blue "#212f51" "#243357" "#27375e" "#2a3b65" "#2c3f6c" "#2f4372" "#324779" "#354b80" "#384F87" "#41578d" "#4b6093" "#556999" "#5f729f" "#697ba5" "#7383ab" "#7d8cb1" "#8795b7")
;;  (magenta "#2e3e5a" "#324362" "#364869" "#3a4e71" "#3e5378" "#425880" "#465d87" "#4a628f" "#4E6897" "#566f9c" "#5f77a1" "#687ea6" "#7186ab" "#7a8db1" "#8395b6" "#8b9cbb" "#94a4c0")
;;  (cyan "#74186d" "#7e1a76" "#871b7f" "#911e88" "#9b2091" "#a4229a" "#ae24a3" "#b826ac" "#C228B6" "#c532b9" "#c83dbd" "#cb48c0" "#ce52c4" "#d15dc8" "#d468cb" "#d773cf" "#da7ed3")
;;  (white "#62717c" "#6a7a86" "#728490" "#7b8d9b" "#8397a5" "#8ba0af" "#93aaba" "#9bb3c4" "#a4bdcf" "#a8c0d1" "#adc3d3" "#b1c6d6" "#b6cad8" "#bacddb" "#bfd0dd" "#c3d4df" "#c8d7e2"))
;;
(def-doom-theme doom-ewal
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '((string (ewal-get-color 'background 0)) nil       nil            ))
   (bg-alt     '("#21242b" nil       nil            ))
   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base2      '("#202328" "#2e2e2e" "brightblack"  ))
   (base3      '("#23272e" "#262626" "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f" "brightblack"  ))
   (base5      '("#5B6268" "#525252" "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
   (base7      '("#9ca0a4" "#979797" "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
   (fg         '("#bbc2cf" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#5B6268" "#2d2d2d" "white"        ))

   (grey       base4)
   (red        '("#ff6c6b" "#ff6655" "red"          ))
   (orange     '("#da8548" "#dd8844" "brightred"    ))
   (green      '("#98be65" "#99bb66" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ECBE7B" "#ECBE7B" "yellow"       ))
   (blue       '("#51afef" "#51afef" "brightblue"   ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (magenta    '("#c678dd" "#c678dd" "brightmagenta"))
   (violet     '("#a9a1e1" "#a9a1e1" "magenta"      ))
   (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-ewal-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-ewal-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-ewal-brighter-modeline)
   (-modeline-pad
    (when doom-ewal-padded-modeline
      (if (integerp doom-ewal-padded-modeline) doom-ewal-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-ewal-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

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
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; Doom-ewal-theme.el ends here
