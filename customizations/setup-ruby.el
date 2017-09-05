(use-package rbenv
  :ensure t
  :defer t)

(use-package robe
  :ensure t)
(use-package inf-ruby
  :ensure t
  :config
  (setq inf-ruby-default-implementation "pry"))
(use-package yard-mode
  :ensure t)

(defun ome-ruby-mode-setup ()
  "Ah, this huge auto-mode-alist list comes from emacs prelude"
  (setq rbenv-installation-dir "/usr/local/bin/rbenv")
  (setq ruby-indent-level 2)
  (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode)))

(defun ome-inf-ruby-setup ()
  (require 'inf-ruby)
  (define-key inf-ruby-minor-mode-map (kbd "C-c C-z") 'run-ruby)
  (define-key inf-ruby-minor-mode-map (kbd "C-c C-b") 'ruby-send-buffer)
  (when (executable-find "pry")
    (add-to-list 'inf-ruby-implementations '("pry" . "pry"))
    (setq inf-ruby-default-implementation "pry")))

(add-hook 'ruby-mode-hook (lambda ()
                            (robe-mode)
                            (global-rbenv-mode)
                            (yard-mode)
                            (smartparens-mode)
                            (robe-start)
                            (flycheck-mode)
                            (smartparens-mode)
                            (ome-inf-ruby-setup)
                            ))
;; The builtin ruby-mode in Emacs is quite simple, it just provides some simple
;; functions for font-locking, indentation and code navigation.
;; Setup ruby-mode to auto load under any of these circumstances

(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))


(ome-ruby-mode-setup)

;; (defun ome-robe-mode-setup ()
;;   (add-hook 'robe-mode-hook 'ac-robe-setup)
;;   (add-to-list 'ac-modes 'inf-ruby-mode)
;;   (add-hook 'inf-ruby-mode-hook 'ac-robe-setup))
;; (setq rbenv-installation-dir "/usr/local/bin/rbenv")
;; (rbenv-use-corresponding)

