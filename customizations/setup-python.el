;; Set up python
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-hook 'py-mode-hook 'subword-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(setq py-indent-level 4)

;; (add-hook
;;  'python-mode
;;  (lambda ()
;;    ;; Make sure `ac-source-chunk-list' comes first.
;;    (setq ac-sources (append '(ac-source-chunk-list) ac-sources))
;;    (setq ac-chunk-list
;;          '("os.path.abspath" "os.path.altsep" "os.path.basename"))))

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
;; don't split windows
(setq py-split-windows-on-execute-p nil)
;; try to automagically figure out indentation
(setq py-smart-indentation t)



