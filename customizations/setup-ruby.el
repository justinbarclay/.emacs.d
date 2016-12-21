(require 'smartparens-config)
(require 'rbenv)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'ruby-mode-hook 'smartparens-mode)
(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'ruby-mode-hook 'global-rbenv-mode)
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'robe-start)

;; The builtin ruby-mode in Emacs is quite simple, it just provides some simple 
;; functions for font-locking, indentation and code navigation.
;; Setup ruby-mode to auto load under any of these circumstances
(defun ome-ruby-mode-setup ()
  ;; Ah, this huge auto-mode-alist list comes from emacs prelude
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
  (add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook
            (lambda ()
              (indent-guide-mode 1))))

(ome-ruby-mode-setup)

;; (setq rbenv-installation-dir "/usr/local/rbenv")
;; (rbenv-use-corresponding)

(setq ruby-indent-level 4)

