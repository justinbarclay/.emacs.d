(use-package paredit
  :defer t
  :ensure t
  :init
  (defun enable-paredit ()
    (turn-off-smartparens-mode)
    (paredit-mode))
  (progn
    (add-hook 'clojure-mode-hook (lambda () (enable-paredit)))
    (add-hook 'emacs-lisp-mode-hook (lambda () (enable-paredit)))
    (add-hook 'common-lisp-mode-hook (lambda () (enable-paredit)))
    (add-hook 'scheme-mode-hook (lambda () (enable-paredit)))
    (add-hook 'lisp-mode-hook (lambda () (enable-paredit)))))

(use-package parinfer
  :ensure t
  :bind
  ("C-t" . parinfer-toggle-mode)
  :init
  ;; (defun enable-parinfer ()
  ;;   (turn-off-smartparens-mode)
  ;;   (parinfer-mode))
  ;; (progn
  ;;   (add-hook 'clojure-mode-hook (lambda () (enable-parinfer)))
  ;;   (add-hook 'emacs-lisp-mode-hook (lambda () (enable-parinfer)))
  ;;   (add-hook 'common-lisp-mode-hook (lambda () (enable-parinfer)))
  ;;   (add-hook 'scheme-mode-hook (lambda () (enable-parinfer)))
  ;;   (add-hook 'lisp-mode-hook (lambda () (enable-parinfer))))
  :config
  (setq parinfer-auto-switch-indent-mode t)
  (setq parinfer-extensions
        '(defaults       ; should be included.
           pretty-parens  ; different paren styles for different modes.
           paredit        ; Introduce some paredit commands.
           smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
           ;;one
           smart-yank)))   ; Yank behavior depend on mode


;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(use-package eldoc
  :ensure t
  :config
  (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round)
  (global-eldoc-mode))

;;Have a live repl available for when developing in lisp
;; Setup from OME https://github.com/xiaohanyu/oh-my-emacs/blob/master/modules/ome-common-lisp.org

(setq inferior-lisp-program (executable-find "sbcl"))

(use-package slime
  :ensure t
  :init
  (use-package slime-company
    :ensure t
    :config
    (setq slime-contribs '(slime-fancy
                           slime-autodoc
                           slime-)))
  (add-hook 'lisp-mode-hook 'slime-mode)
  (add-hook 'lisp-mode-hook (lambda () (with-current-buffer (buffer-name)
                                         (let (old-window selected-window)
                                           (slime)
                                           (delete-other-windows old-window)
                                           (window-buffer old-window))))))
  
