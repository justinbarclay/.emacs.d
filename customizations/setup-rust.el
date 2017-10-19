;;; package --- summary
;;; Commentary:
;;; Code:
(use-package flycheck-rust
  :ensure t
  :commands (flycheck-rust-setup))

;; (use-package racer
;;   :defer t
;;   :commands (racer-mode)
;;   :ensure t
;;   :init
;;   (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package lsp-mode
  :ensure t)
(use-package lsp-rust
  :ensure t)
  
(use-package rust-mode
  :mode "\\.rs\\'"
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'company-mode)
  ;; (add-hook 'rust-mode-hook 'racer-mode)
  ;; (add-hook 'rust-mode-hook 'ycmd-mode)
  (add-hook 'rust-mode-hook 'flycheck-rust-setup)
  :config
  (add-hook 'rust-mode-hook
          '(lambda ()
             ;; (setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
             ;; (setq racer-rust-src-path (concat (getenv "HOME") "/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
             (electric-pair-mode 1)
             (lsp-mode)
	     ))
  (add-hook 'rust-mode-hook
            '(lambda ()
              (local-set-key (kbd "C-c <tab>") #'rust-format-buffer))))


(provide 'setup-rust)
;;; setup-rust.el ends here
