(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(use-package eldoc
  :ensure t
  :config
  (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))


;;Have a live repl available for when developing in lisp
;; Setup from OME https://github.com/xiaohanyu/oh-my-emacs/blob/master/modules/ome-common-lisp.org

(setq inferior-lisp-program (executable-find "sbcl"))

(use-package slime
  :ensure t
  :init
  (use-package slime-company
    :ensure t)
  :config
  (setq slime-contribs '(slime-fancy
                         slime-autodoc
                         slime-company))
                       
  (add-hook 'lisp-mode-hook 'slime-mode)
  (add-hook 'lisp-mode-hook (lambda () (with-current-buffer (buffer-name)
                                         (let (old-window selected-window)
                                           (slime)
                                           (delete-other-windows old-window)
                                           (window-buffer old-window))))))
