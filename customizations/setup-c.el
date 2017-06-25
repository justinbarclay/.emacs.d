(use-package smartparens
  :ensure t)
(use-package c-eldoc
  :ensure t)

(use-package counsel-gtags
  :ensure t)
  ;; :bind (("M-m x" . counsel-gtags-find-definition)
  ;;        ("M-m r" . counsel-gtags-find-reference)
  ;;        ("M-m s" . counsel-gtags-find-symbol)
  ;;        ("M-," . counsel-gtags-go-backward))
  

(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'semantic-mode)
(add-hook 'c-mode-hook 'ycmd-mode)
(add-hook 'c-mode-hook 'counsel-gtags-mode)
(add-hook 'c++-mode-hook 'counsel-gtags-mode)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c-mode-hook 'smartparens-mode)

(eval-after-load 'c-mode '(setq-local eldoc-documentation-function #'ggtags-eldoc-function))

(use-package ggtags
  :ensure t)
  ;;:bind)
  ;; (("M-m g s" . ggtags-find-other-symbol)
  ;;  ("M-m g h" . ggtags-view-tag-history)
  ;;  ("M-m g r" . ggtags-find-reference)
  ;;  ("M-m g f" . ggtags-find-file)
  ;;  ("M-m g c" . ggtags-create-tags)
  ;;  ("M-m g u" . ggtags-update-tags)
;;   ("M-," . pop-tag-mark))


;;Semantic is a package that provides language-aware editing commands based on source code parsers. Parsing is a process of analyzing source code based on programming language syntax. Emacs understands your source code through this process to provides features such as contextual code completion, code navigation.
(semantic-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
