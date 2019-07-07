;;; ~/dotfiles/doom/autoload.el -*- lexical-binding: t; -*-
(defun jscott/open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    )))
(defun jscott/term-send-up () (interactive) (term-send-raw-string "\e[A"))
(defun jscott/term-send-down () (interactive) (term-send-raw-string "\e[B"))
(defun jscott/term-send-right () (interactive) (term-send-raw-string "\e[C"))
(defun jscott/term-send-left () (interactive) (term-send-raw-string "\e[D"))
