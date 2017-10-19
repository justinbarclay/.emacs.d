;; Customizations relating to editing a buffer.

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

(set-default 'truncate-lines t)
(define-key global-map (kbd "RET") 'newline-and-indent)
;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; comments
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; use 4 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 4)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;;(setq electric-indent-mode nil)
;;(use-package electric-indent-mode)

(use-package electric
  :demand t
  :config (electric-indent-mode t))



(use-package flycheck-pos-tip
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-standard-error-navigation nil)
  (when 'display-graphic-p (selected-frame)
        (eval-after-load 'flycheck
          (flycheck-pos-tip-mode))))
;; flycheck errors on a tooltip (doesnt work on console)


(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package origami
  :ensure t
  :bind ("C-s-<tab>" . origami-recursively-toggle-node)
  :init
  (add-hook 'prog-mode-hook 'origami-mode))
(use-package hungry-delete
  :ensure t
  :demand t
  :init
  (global-hungry-delete-mode))

(use-package smartparens
  :ensure t
  :bind (:map smartparens-mode-map
          ("C-)" . sp-forward-slurp-sexp)
          ("C-(" . sp-backward-slurp-sexp)
          ("C-}" . sp-forward-barf-sexp)
          ("C-{" . sp-backward-barf-sexp))
  :config
  (setq sp-escape-wrapped-region nil))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

  ;; Key bindings for multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("<s-mouse-1>" . mc/add-cursor-on-click))
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this))

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'less-mode-hook 'rainbow-mode))

(use-package dash-at-point
  :ensure t
  :config
  (global-set-key "\C-cd" 'dash-at-point)
  (global-set-key "\C-ce" 'dash-at-point-with-docset)
  (add-to-list 'dash-at-point-mode-alist '(ruby-mode . ("ruby" "rails"))))
