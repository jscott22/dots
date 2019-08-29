(setq doom-font (font-spec :family "UbuntuMono NF" :size 18))
(setq doom-theme 'doom-wal)
(doom/reload-font)

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(def-package! ewal
  :init (ewal-load-ewal-colors))

(def-package! lsp-mode
  :init (add-to-list 'exec-path "/home/blaeni/elixir-ls/release")
  :hook
  (elixir-mode . lsp)
  :config
  (setq lsp-enable-eldoc t
        lsp-enable-completion-at-point t))

(def-package! company-lsp
  :config
  (setq company-lsp-async t
        company-lsp-enable-snippet t))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1))

(add-hook 'elixir-mode-hook
          (lambda ()
            (lsp)
            (set-company-backend! 'elixir-mode '(company-lsp company-yasnippet))))

(add-hook 'elixir-mode-hook
          (lambda ()
            (lsp)
            (flycheck-add-next-checker 'lsp-ui 'elixir-credo)))

;; (add-hook 'elixir-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'elixir-format nil t)))

(def-package! org
  :init
  (setq org-directory "~/Dropbox/Documents/Org"
        org-agenda-files (list org-directory)))

(after! org
  (setq org-capture-templates
        '(("t" "Task" entry
           (file+headline "inbox.org" "Tasks")
           "** TODO %? %^G \n %U" )
          ("h" "Home")
          ("ht" "Home - Task" entry
           (file+headline "personal.org" "Tasks")
           "** TODO %? %^G \n %U" ))))

(after! counsel
  (map!
   [remap org-capture]              #'org-capture))

(after! org
  (setq org-log-reschedule 'note)
  (setq org-log-done 'time))

(after! org
  (setq org-default-notes-file (concat org-directory "/inbox.org")))

(after! org
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil))

(after! org
  (setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "WAITING(w@/!)" "MAYBE(m@)" "PROJ(p)" "|" "DONE(d!)" "CANCELED(c@/!)" ))))

;foo
(def-package! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("NEXT"  . ,(face-foreground 'warning))
          ("WAITING" . ,(face-foreground 'warning))
          ("MAYBE" . ,(face-foreground 'warning))
          ("PROJ" . ,(face-foreground 'warning))
          ("NOTE"  . ,(face-foreground 'success)))))

(defun org-update-cookies-after-save()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (org-update-statistics-cookies "ALL")))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'org-update-cookies-after-save nil 'make-it-local)))

;(setq notmuch-message-deleted-tags '("+trash" "+deleted" "-inbox" "-unread"))
(setq notmuch-search-oldest-first nil)

(after! notmuch
  (setq send-mail-function 'sendmail-send-it
        message-sendmail-f-is-evil 't
        mail-specify-envelope-from 't
        mail-envelope-from 'header
        mail-host-address "gmail.com"
        user-full-name "Jason Scott"
        notmuch-always-prompt-for-sender 't
        sendmail-program "/usr/local/bin/msmtp"))

;;;###autoload
(defun +notmuch/unsubscribe ()
  (interactive)
  (notmuch-show-move-to-message-bottom)
  (when (search-backward "unsubscribe" (notmuch-show-message-top))
    (if (ffap-url-at-point)
        (goto-char (car ffap-string-at-point-region)))

    (ffap-next-url)))

;;;###autoload
(defun +notmuch/find-similar ()
  (interactive)
  (let ((sender (notmuch-show-get-from)))
    (notmuch-bury-or-kill-this-buffer)
    (notmuch-search sender)))

;;;###autoload
(defun +notmuch/delete-all ()
  (interactive)
  (notmuch-search-tag-all '("+trash" "-inbox" "-unread" "-important" "-flagged")))

(def-package! calfw-org
  :config
  (setq +calendar-org-gcal-secret-file (concat doom-private-dir "calendar-secret.el")))

(defun jscott/open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    )))

(def-package! atomic-chrome
  :init
  (atomic-chrome-start-server))

(def-package! org-mind-map
  :init
  (require 'ox-org)
  :config
  (setq org-mind-map-engine "dot"
        org-mind-map-include-text t))

(map!
 (:leader
   (:prefix ("a" . "apps")
     (:prefix ("c" . "calendar")
       :desc "Sync events" "s" #'org-gcal-sync
       :desc "Fetch events" "f" #'org-gcal-fetch
       :desc "Open calendar" "o" #'=calendar
       )
     (:prefix ("n" . "notmuch")
       :desc "Delete all" "d" #'+notmuch/delete-all
       :desc "Find similar" "f" #'+notmuch/find-similar
       :desc "Jump to saved search" "j" #'notmuch-jump-search
       :desc "Open notmuch" "o" #'=notmuch
       :desc "Unsubscribe" "u" #'+notmuch/unsubscribe
       :desc "Hydra" "h" #'+notmuch/hydra
       ))))

(map!
 :map evil-normal-state-map
 (:prefix "["
   "w"  #'previous-multiframe-window
   "W"  #'+workspace/switch-left)
 (:prefix "]"
   "w"  #'next-multiframe-window
   "W"  #'+workspace/switch-right))

;foo
(add-hook! 'eshell-mode-hook
 (map! :map eshell-mode-map
 :nv "M-j" #'eshell-next-input
 :nv "M-k" #'eshell-previous-input))

(map!
  :map org-mode-map
  (:localleader
    "t" nil))

;;;###autoload
;; (defun +current-line-empty-p ()
;;   (save-excursion
;;     (beginning-of-line)
;;     (looking-at "[[:space:]]*$")))

;; ;;;###autoload
;; (defun +org/insert-checkbox ()
;;   (interactive)
;;   (unless (+current-line-empty-p)
;;     (+default/newline-below))
;;   (insert "- [ ]"))

;;;(map!
;;; :map org-mode-map
;;; (:localleader
  ;;; (:prefix ("i" . "insert")
    ;;; :desc "Checkbox" "c" #'+org/insert-checkbox
;;;     :desc "Heading" "h" #'evil-org-org-insert-heading-respect-content-below
  ;;;   :desc "Link" "l" #'org-insert-link
;;;     :desc "Note" "n" #'org-add-note
  ;;;   :desc "Subheading" "s" #'org-insert-subheading
    ;;; :desc "Tag" "t" #'counsel-org-tag)
;;;   (:prefix ("t" . "toggle")
  ;;;   :desc "Todo" "t" #'org-todo
    ;;; :desc "Checkbox" "c" #'org-toggle-checkbox)))

(map!
 :map org-mode-map
 :nv "g-" #'org-narrow-to-subtree)

(after! notmuch
  (map!
   :map notmuch-search-mode-map
   :nv "d" #'+notmuch/search-delete)

  (map!
   :map notmuch-tree-mode-map
   :nv "d" #'+notmuch/tree-delete)
  )

(def-package! nand2tetris
  :config
  (setq nan2tetris-core-base-dir "~/nand2tetris"
        nand2tetris-hardware-simulator "~/nand2tetris/tools/HardwareSimulator.sh"))

(def-package! evil
  :init
  (setq evil-want-C-i-jump nil))
