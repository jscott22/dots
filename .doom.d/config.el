(after! notmuch
  (setq notmuch-multipart/alternative-discouraged '("text/plain" "multipart/related")))

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
