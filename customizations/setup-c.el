(use-package c-eldoc
  :ensure t)

(use-package counsel-gtags
  :ensure t
  :bind (("M-," . counsel-gtags-find-definition))
  :config
  (setq counsel-gtags-auto-update t))
  ;;        ("M-m r" . counsel-gtags-find-reference)
  ;;        ("M-m s" . counsel-gtags-find-symbol)
;;        ("M-," . counsel-gtags-go-backward))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-hook 'ggtags-mode))
  

;; 
(progn ; C mode hook
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'semantic-mode)
  (add-hook 'c-mode-hook 'ycmd-mode)
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook 'counsel-gtags-mode)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  (add-hook 'c-mode-hook 'smartparens-mode))

(eval-after-load 'c-mode '(setq-local eldoc-documentation-function #'ggtags-eldoc-function))


  ;;:bind)
  ;; (("M-m g s" . ggtags-find-other-symbol)
  ;;  ("M-m g h" . ggtags-view-tag-history)
  ;;("M-m g r" . ggtags-find-reference)
  ;;  ("M-m g f" . ggtags-find-file)
  ;;  ("M-m g c" . ggtags-create-tags)
  ;;("M-m g u" . ggtags-update-tags))
;;   ("M-," . pop-tag-mark))

;; Set C tabs length
(setq-default c-basic-offset 4)
