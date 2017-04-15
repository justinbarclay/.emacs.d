;;; package --- summary
;;; Commentary:
;;; Code:
;; (use-package rust-mode
;;   :ensure t
;;   :mode "\\.rs\\'"
;;   :init (add-hook 'rust-mode-hook  #'racer-mode)
;;         (add-hook 'rust-mode-hook  #'company-mode)
;;         (add-hook 'rust-mode-hook
;;           '(lambda ()
;;              (setq racer-cmd (concat (getenv "HOME") "/.rust-dev/racer/target/release/racer"))
;;              (setq racer-rust-src-path (concat (getenv "HOME") "/.rust-dev/rust/src")
;;                    (local-set-key (kbd "TAB") #'company-indent-or-complete-common))
;;              (electric-pair-mode 1))))
;; (use-package flycheck-rust
;;   :ensure t
;;   :mode "\\.rs\\'")
  
;; (use-package racer
;;   :defer t
;;   :mode "\\.rs\\'"
;;   :ensure t
;;   :init (add-hook 'racer-mode-hook #'eldoc-mode))

(require 'company)
(require 'racer)
(require 'rust-mode)
(require 'electric)
(require 'eldoc)
(require 'flycheck-rust)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook  #'company-mode)
(add-hook 'rust-mode-hook  #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook
          '(lambda ()
             (setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
             (setq racer-rust-src-path (concat (getenv "HOME") "/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
             (electric-pair-mode 1)))

(provide 'setup-rust)
;;; setup-rust.el ends here
