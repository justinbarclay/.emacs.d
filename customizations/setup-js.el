;;; Code:
;; javascript / html
;; Package inspired by https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html
(use-package indium
  :ensure t
  :defer t
  :config
  (add-hook 'indium-update-script-source-hook
        (lambda (url)
          (indium-eval (format "window.dispatchEvent(new CustomEvent('patch', {detail: {url: '%s'}}))"
                               url))))
  (indium-interaction-mode))

(use-package js2-mode
  :ensure t
  :demand t
  :mode "\\.js\\'"
  :config
  (add-hook 'js-mode-hook 'subword-mode)
  (add-hook 'html-mode-hook 'subword-mode)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode)))
  (setq js-indent-level 4)
  (require 'indium)
  (add-hook 'js-mode-hook #'indium-interaction-mode))

(use-package js2-refactor
  :ensure t
  :config
   (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
   (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package xref-js2
  :bind
  ("M-." . nil)
  :ensure t)
  
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (add-hook 'js-mode-hook 'subword-mode)
;; (add-hook 'html-mode-hook 'subword-mode)

(use-package tagedit
  :ensure t)
(use-package sgml-mode
  :config
  (require 'tagedit)
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'html-mode-hook (lambda () (tagedit-mode 1))))


;; coffeescript
(use-package coffee-mode
  :mode "\\.coffee$"
  :config
  (add-to-list 'company-backends 'company-tern)
  (custom-set-variables '(coffee-tab-width 2))
  (add-hook 'coffee-mode-hook 'subword-mode)
  (add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
  (add-hook 'coffee-mode-hook
            (defun coffee-mode-newline-and-indent ()
              (define-key coffee-mode-map "\C-j" 'coffee-newline-and-indent)
              (setq coffee-cleanup-whitespace nil))))

(use-package company-tern
  :bind
  ("M-." . nil)
  ("M-," . nil)
  :ensure t
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-tern-property-marker " <p>"))
;; (add-to-list 'auto-mode-alist '("\\.coffee.erb$" . coffee-mode))
;; (add-hook 'coffee-mode-hook 'subword-mode)
;; (add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
;; (add-hook 'coffee-mode-hook
;;           (defun coffee-mode-newline-and-indent ()
;;             (define-key coffee-mode-map "\C-j" 'coffee-newline-and-indent)
;;             (setq coffee-cleanup-whitespace nil)))


;; (use-package rjsx-mode
;;   :ensure t
;;   :mode "\\.js$"
;;   :config
;;   (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
;;   (add-hook 'rjsx-mode 'indium-interaction-mode)
;;   (add-hook 'rjsx-mode 'ycmd-mode))


;; Neat helper functions
(defun js2-unused--find-definitions ()
  ;; Reset the value before visiting the AST
  (setq js2-unused-definitions nil)
  (js2-visit-ast js2-mode-ast
                 #'js2-unused-visitor))

(defun js2-unused-visitor (node end-p)
  "Add NODE's name to `js2-unused-definitions` if it is a function."
  (unless end-p
    (cond
     ;; assignment to a function
     ((and (js2-assign-node-p node)
           (js2-function-node-p (js2-assign-node-right node)))
      (push (js2-node-string (js2-assign-node-left node)) js2-unused-definitions))
     ;; function declaration (skipping anonymous ones)
     ((js2-function-node-p node)
      (if-let ((name (js2-function-name node)))
          (push name js2-unused-definitions))))
    t))
(defun js2-unused-functions ()
  (interactive)
  ;; Make sure that JS2 has finished parsing the buffer
  (js2-mode-wait-for-parse
   (lambda ()
     ;; Walk the AST tree to find all function definitions
     (js2-unused--find-definitions)
     ;; Use xref-js2 to filter the ones that are not referenced anywhere
     (let ((unused (seq-filter (lambda (name)
                                 (null (xref-js2--find-references
                                        (js2-unused--unqualified-name name))))
                               js2-unused-definitions)))
       ;; If there are unreferenced function, display a message
       (apply #'message (if unused
                            `("Unused functions in %s: %s "
                              ,(file-name-nondirectory buffer-file-name)
                              ,(mapconcat #'identity unused " "))
                          '("No unused function found")))))))
                          
(defun js2-unused--unqualified-name (name)
  "Return the local name of NAME.
foo.bar.baz => baz"
  (save-match-data
    (if (string-match "\\.\\([^.]+\\)$" name)
        (match-string 1 name)
      name)))
