(setq gc-cons-threshold 1000000000)
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 10000000)
   (message "gc-cons-threshold restored to %S"
            gc-cons-threshold)))

(setq
 lexical-binding t
 load-prefer-newer t)

(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq package-user-dir "~/.emacs.d/elpa")
(setq package-archives
             '(("melpa" . "http://melpa.org/packages/")
              ("org" . "https://orgmode.org/elpa/")
              ("tromey" . "http://tromey.com/elpa/")))

(setq user-full-name "Justin Barclay"
      user-mail-address "justinbarclay@gmail.com")

(setq package-enable-at-startup nil
      package--init-file-ensured t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defmacro use-package-with-elpa ()
  "Set up use-package to optimal usage with package.el.

For full documentation on the meaning and usage of this, please
consult the README file that came with this file at the section
called `Byte-compiling with Package.el'."
  '(progn
     ;; Disable package initialize after us.  We either initialize it
     ;; anyway in case of interpreted .emacs, or we don't want slow
     ;; initizlization in case of byte-compiled .emacs.elc.
     (setq package-enable-at-startup nil)
     ;; Set use-package-verbose to t for interpreted .emacs,
     ;; and to nil for byte-compiled .emacs.elc.
     (eval-and-compile
       (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
     ;; Add the macro generated list of package.el loadpaths to load-path.
     (mapc (lambda (add) (add-to-list 'load-path add))
           (eval-when-compile
             (setq use-package-always-ensure t)
             (let ((package-user-dir-real (file-truename package-user-dir)))
               ;; The reverse is necessary, because outside we mapc
               ;; add-to-list element-by-element, which reverses.
               (nreverse (apply #'nconc
                                ;; Only keep package.el provided loadpaths.
                                (mapcar (lambda (path)
                                          (if (string-prefix-p package-user-dir-real path)
                                              (list path)
                                            nil))
                                        load-path))))))))

(use-package-with-elpa)

(progn ;'use-package
  (require 'use-package)
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-enable-imenu-support t))

;; (use-package ess-site                   
;;   :disabled
;;   :commands R)

(use-package org
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :init
  (setq truncate-lines t
        global-company-modes '(not org-mode))
  :config
  (progn
    (setq org-startup-truncated nil)
    (setq org-capture-templates
          '(("a" "Appointment" entry (file+headline  "~/Dropbox/orgfiles/gcal.org" "Appointments")
             "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")
            ("l" "Link" entry (file+headline "~/Dropbox/orgfiles/links.org" "Links")
             "* %? %^L %^g \n%T" :prepend)))
    (setq org-agenda-files (list "~/Dropbox/orgfiles/gcal.org"))
    (custom-set-variables
     '(org-directory "~/Dropbox/orgfiles")
     '(org-default-notes-file (concat org-directory "/notes.org"))
     '(org-export-html-postamble nil)
     '(org-hide-leading-stars t)
     '(org-startup-folded (quote overview))
     '(org-startup-indented t))))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package toc-org
  :hook (org-mode-hook . toc-org-enable))

(use-package eshell
  :init
  (add-hook 'eshell-mode-hook
                   #'company-mode)
  :config
  (progn
    (eval-after-load 'esh-opt
      '(progn
         (require 'em-prompt)
         (require 'em-term)
         (require 'em-cmpl)
         (setenv "PAGER" "cat")
         (add-to-list 'eshell-visual-commands "ssh")
         (add-to-list 'eshell-visual-commands "htop")
         (add-to-list 'eshell-visual-commands "top")
         (add-to-list 'eshell-visual-commands "tail")
         (add-to-list 'eshell-visual-commands "vim")
         (add-to-list 'eshell-visual-commands "bower")
         (add-to-list 'eshell-visual-commands "npm")

         (add-to-list 'eshell-command-completions-alist
                      '("gunzip" "gz\\'"))
         (add-to-list 'eshell-command-completions-alist
                      '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))))

;; Magit is an Emacs interface to Git.
;; (It's awesome)
;; https://github.com/magit/magit
(use-package magit
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep))
  :init
  (progn
    ;; magit extensions

    ;; make magit status go full-screen but remember previous window
    ;; settings
    ;; from: http://whattheemacsd.com/setup-magit.el-01.html
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    ;; Close popup when commiting - this stops the commit window
    ;; hanging around
    ;; From: http://git.io/rPBE0Q
    (defadvice git-commit-commit (after delete-window activate)
      (delete-window))

    (defadvice git-commit-abort (after delete-window activate)
      (delete-window))

    ;; these two force a new line to be inserted into a commit window,
    ;; which stops the invalid style showing up.
    ;; From: http://git.io/rPBE0Q
    (defun magit-commit-mode-init ()
      (when (looking-at "\n")
        (open-line 1)))

    (add-hook 'git-commit-mode-hook 'magit-commit-mode-init))
  :config
  (progn
    ;; restore previously hidden windows
        ;; major mode for editing `git rebase -i`
    (defadvice magit-quit-window (around magit-restore-screen activate)
      (let ((current-mode major-mode))
        ad-do-it
        ;; we only want to jump to register when the last seen buffer
        ;; was a magit-status buffer.
        (when (eq 'magit-status-mode current-mode)
          (jump-to-register :magit-fullscreen))))

    (defun magit-maybe-commit (&optional show-options)
      "Runs magit-commit unless prefix is passed"
      (interactive "P")
      (if show-options
          (magit-key-mode-popup-committing)
        (magit-commit)))

    (define-key magit-mode-map "c" 'magit-maybe-commit)

    ;; magit settings
    (setq
     ;; use ido to look for branches
     magit-completing-read-function  'ivy-completing-read
     ;; don't put "origin-" in front of new branch names by default
     magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
     ;; open magit status in same window as current buffer
     magit-status-buffer-switch-function 'switch-to-buffer
     ;; highlight word/letter changes in hunk diffs
     magit-diff-refine-hunk t
     ;; ask me if I want to include a revision when rewriting
     magit-rewrite-inclusive 'ask
     ;; ask me to save buffers
     magit-save-some-buffers nil
     ;; pop the process buffer if we're taking a while to complete
     magit-process-popup-time 10
     ;; ask me if I want a tracking upstream
     magit-set-upstream-on-push 'askifnotset)))

(use-package magit-blame
  :ensure nil
  :bind ("C-c C-g b" . magit-blame-mode))

(tool-bar-mode -1)

(when (display-graphic-p) ; Start full screen
  (add-to-list 'default-frame-alist '(fullscreen . t))
  (x-focus-frame nil))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq-default frame-title-format "%b (%f)")

(set-face-attribute 'default nil
                    :family "Inconsolata for Powerline" :height 180 :weight 'normal)

(global-set-key (kbd "s-t") '(lambda () (interactive)))

(blink-cursor-mode 0)

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
 select-enable-clipboard t

 ;; I'm actually not sure what this does but it's recommended?
 select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

(setq ring-bell-function 'ignore)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(use-package dracula-theme
  :demand t
  :config
  (load-theme 'dracula t))

(use-package powerline
  :config
  ;;(powerline-center-theme)
  (setq powerline-default-separator 'wave))

(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq spaceline-byte-compile nil))

(use-package all-the-icons)

(use-package spaceline-all-the-icons
  :hook (after-init . spaceline-all-the-icons-theme)
  :config
  (progn
    (custom-set-faces '(spaceline-highlight-face ((t (:background "#cb619e"
                                                                  :foreground "#f8f8f2"
                                                                  :inherit 'mode-line))))
                      '(powerline-active2 ((t (:background "#44475a"
                                                           :foregound "#50fa7b"
                                                           :inherit 'mode-line))))
                      '(mode-line ((t (:background "#282a36"
                                                   :foregound "#50fa7b"
                                                   :inherit 'mode-line))))
                      '(powerline-active1 ((t (:background "#6272a4"
                                                           :foregound "#50fa7b"
                                                           :inherit 'mode-line)))))
    (setq spaceline-all-the-icons-separator-type 'wave)))

(global-display-line-numbers-mode)
(set-default 'display-line-numbers-type 'visual)
(setq display-line-numbers-current-absolute t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
   (custom-set-faces 
    '(rainbow-delimiters-depth-0-face ((t (:foreground "saddle brown"))))
    '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
    '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
    '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
    '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
    '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
    '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
    '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
    '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
    '(rainbow-delimiters-unmatched-face ((t (:foreground "black"))))))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("a" . dired-find-file)))

(use-package diminish
  :demand t
  :config (progn
            ;;            (diminish 'auto-revert-mode)
            ;;            (diminish 'outline-minor-mode)
            ;;            (diminish 'amd-mode)
            (diminish 'js2-refactor-mode)
            (diminish 'tern-mode)))

(when (memq system-type '(windows-nt))
  (add-to-list gnutls-trustfile ((expand-file-name "~/.cert/cacert.pm")))
  (exec-path-from-shell-initialize)
  (setq explicit-shell-file-name "c:/windows/system32/bash.exe")
  (setq shell-file-name "bash")
  (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
  (setenv "SHELL" shell-file-name)
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package dired+
  :config
  (setq dired-dwim-target t)
  (setq dired-recursive-copies `always))

(use-package recentf
  :config
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode 1)
  (setq recentf-max-menu-items 40))

(use-package projectile
  :commands
  (projectile-find-file projectile-switch-project)
  :diminish
  (projectile-mode)
  :config
  (progn
    (setq projectile-completion-system 'ivy)
    (setq projectile-enable-caching t)))

(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-initial-inputs-alist nil)
    (projectile-global-mode)
    (counsel-mode)))

(use-package counsel
  :after ivy
  :init
  (progn
    (setq counsel-grep-base-command
          "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))
  :bind
  (("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file)
    ("C-c p f" . counsel-projectile-find-file)
    ("C-c p d" . counsel-projectile-find-dir)
    ("C-c p p" . counsel-projectile-switch-project)
    ("<f1> f" . counsel-describe-function)
    ("<f1> v" . counsel-describe-variable)
    ("<f1> l" . counsel-load-library)
    ("<f2> i" . counsel-info-lookup-symbol)
    ("<f2> u" . counsel-unicode-char)
    ("C-c k" . counsel-rg)))

(use-package counsel-projectile
  :after projectile
  :commands (counsel-projectile-switch-project counsel-projectile-find-file counsel-projectile-find-dir)
  :bind)

(use-package swiper
  :after ivy
  :bind ("C-s" . swiper))

(use-package treemacs
  :config
  (progn
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind
  (:map global-map
        ([f8]        . treemacs-toggle)
        ("<C-M-tab>" . treemacs-toggle)
        ("M-0"       . treemacs-select-window)
        ("C-c 1"     . treemacs-delete-other-windows)))

(use-package treemacs-projectile
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

(use-package avy
  :ensure t
  :bind ("C-c s" . avy-goto-char))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("<s-mouse-1>" . mc/add-cursor-on-click))
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this))

(define-key global-map (kbd "RET") 'newline-and-indent)

(set-default 'truncate-lines t)

(global-hl-line-mode 1)

(show-paren-mode 1)

(setq-default indent-tabs-mode nil)

(setq tab-width 2)

(save-place-mode 1)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :bind (:map smartparens-mode-map
          ("C-)" . sp-forward-slurp-sexp)
          ("C-(" . sp-backward-slurp-sexp)
          ("C-}" . sp-forward-barf-sexp)
          ("C-{" . sp-backward-barf-sexp))
  :config
  (setq sp-escape-wrapped-region nil))

(use-package hungry-delete
  :hook (prog-mode . global-hungry-delete-mode))

(use-package origami
  :bind ("C-s-<tab>" . origami-recursively-toggle-node)
  :hook (prog-mode . origami-mode))

(use-package dash-at-point
  :bind
  (("C-c d" . dash-at-point)
   ("C-c e" . dash-at-point-with-docset))
  :config
  (add-to-list 'dash-at-point-mode-alist '(ruby-mode . ("ruby" "rails"))))

(use-package undo-tree
  :demand t
  :config
  (global-undo-tree-mode))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

(use-package ws-butler
  :commands (ws-butler-global-mode)
  :hook (after-init . (lambda () (ws-butler-global-mode 1))))

(use-package flycheck-pos-tip)

(use-package flycheck
  :after flycheck-pos-tip
  :demand t
  :config
  (progn
    (global-flycheck-mode)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-standard-error-navigation nil)
    (when 'display-graphic-p (selected-frame)
      (eval-after-load 'flycheck
      (flycheck-pos-tip-mode)))))

(use-package semantic 
  :config
  (semantic-mode 1)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1))

(use-package company
  :commands (global-company-mode)
  :bind
  (;;("C-<tab>" . company-capf)
   :map company-mode-map
   (("M-h" . company-quickhelp-manual-begin)))
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (progn
    (eval-after-load 'company
      '(push 'company-robe company-backends))
    (setq company-idle-delay 0.5)
    (setq company-frontends
          '(company-pseudo-tooltip-unless-just-one-frontend
            company-preview-frontend
            company-echo-metadata-frontend))
    (setq company-auto-complete t)
    (setq company-tooltip-align-annotations t)))

(use-package ycmd
  :commands (ycmd-mode)
  :config
  (progn
    (set-variable 'ycmd-server-command '("python" "/Users/Justin/ycmd/ycmd"))
    (set-variable 'ycmd-extra-conf-whitelist '("~/*"))
    (set-variable 'ycmd-global-config "~/.ycm_extra_conf.py")
    (require 'ycmd-eldoc)
    (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
    ;;  (setq ycmd-force-semantic-completion t)
    (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)))

(use-package company-ycmd
  :after ycmd
  :config
  (company-ycmd-setup))

(use-package lsp-mode)

(use-package rainbow-mode
  :hook ((css-mode . rainbow-mode)
         (less-mode . rainbow-mode)))

(use-package sass-mode
  :mode "\\.sass\\'")

(progn ; C mode hook
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'semantic-mode)
  (add-hook 'c-mode-hook 'ycmd-mode)
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook 'counsel-gtags-mode)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

(eval-after-load 'c-mode '(setq-local eldoc-documentation-function #'ggtags-eldoc-function))

(setq-default c-basic-offset 2)

(use-package c-eldoc)

(use-package counsel-gtags
  :bind (("M-," . counsel-gtags-find-definition))
  :config
  (setq counsel-gtags-auto-update t))

(use-package ggtags
  :config
  (add-hook 'c-common-mode-hook 'ggtags-mode))

(use-package paredit
  :commands (paredit-mode)
  :hook ((common-lisp-mode . (lambda () (enable-paredit)))
         (scheme-mode . (lambda () (enable-paredit)))
         (lisp-mode . (lambda () (enable-paredit)))))

(use-package parinfer
  :commands (parinfer-mode)
  :bind (:map parinfer-mode-map
              (("C-t" . parinfer-toggle-mode)))
  :init (progn
            (setq parinfer-delay-invoke-threshold 30000)
            (setq parinfer-auto-switch-indent-mode t)
            (setq parinfer-extensions
                  '(defaults       ; should be included.
                     pretty-parens  ; different paren styles for different modes.
                     paredit        ; Introduce some paredit commands.
                     smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
                     lispy
                     ;;one
                     smart-yank))))   ; Yank behavior depend on mode

(use-package lispy
  :commands (lispy-mode))

(use-package eldoc
  :ensure t
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  (global-eldoc-mode))

(use-package slime
  :ensure t
  :init
  (add-hook 'lisp-mode-hook 'slime-mode)
  (add-hook 'lisp-mode-hook (lambda () (with-current-buffer (buffer-name)
                                         (let (old-window selected-window)
                                           (slime)
                                           (delete-other-windows old-window)
                                           (window-buffer old-window))))))

(use-package slime-company
  :after slime
  :config
  (setq slime-contribs '(slime-fancy
                         slime-autodoc)))

(use-package lisp-mode
  :ensure nil
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

(use-package elisp-mode
  :ensure nil
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (enable-paredit))))

(use-package flycheck-joker
  :after (clojure-mode)
  :config
  (require 'flycheck-joker))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ( "\\.cljs\\'" . clojurescript-mode))
  :init
  (progn
    (add-hook 'clojure-mode-hook (lambda () (enable-parinfer)))
    (add-hook 'clojure-mode-hook 'flycheck-mode)
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'clojure-mode-hook 'eldoc-mode)
    (add-hook 'clojure-mode-hook 'subword-mode))
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
    (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
    (setq inferior-lisp-program "lein repl")
    ;;(add-hook 'clojure-mode-hook (lambda () (cider-jack-in)))
    (add-hook 'clojurescript-mode-hook (lambda () (cider-jack-in-clojurescript)))
    (font-lock-add-keywords
     nil
     '(("(\\(facts?\\)"
        (2 font-lock-keyword-face))
       ("(\\(background?\\)"
        (2 font-lock-keyword-face))))
    (electric-pair-mode)
    (setq define-clojure-indent 2)))

(use-package cider
  :hook ((clojure-mode . cider-mode)
         (clojurescript-mode . cider-mode))
  :commands (cider-jack-in cider-jack-in-clojurescript)
  :config
  (progn
    ;; REPL related stuff
    ;; REPL history file

    (defun cider-check-figwheel-requirements ()
      "Check whether we can start a Figwheel ClojureScript REPL."
     t)

    (setq cider-repl-history-file "~/.emacs.d/cider-history")
    ;; nice pretty printing

    (setq cider-repl-use-pretty-printing t)
    ;; nicer font lock in REPL

    (setq cider-repl-use-clojure-font-lock t)
    ;; result prefix for the REPL

    (setq cider-repl-result-prefix ";; => ")
    ;; never ending REPL history

    (setq cider-repl-wrap-history t)
    ;; looong history

    (setq cider-repl-history-size 3000)
    ;; eldoc for clojure

    (add-hook 'cider-mode-hook #'eldoc-mode)
    ;; error buffer not popping up

    (setq cider-show-error-buffer nil)
    ;; go right to the REPL buffer when it's finished connecting
    (setq cider-repl-pop-to-buffer-on-connect nil)
    ;; company mode for completion
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode)
    (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
    ;; key bindings
    ;; these help me out with the way I usually develop web apps
    (defun cider-start-http-server ()
      (interactive)
      (cider-load-current-buffer)
      (let ((ns (cider-current-ns)))
        (cider-repl-set-ns ns)
        (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
        (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))
    (defun cider-refresh ()
      (interactive)
      (cider-interactive-eval (format "(user/reset)")))
    (defun cider-user-ns ()
      (interactive)
      (cider-repl-set-ns "user"))
    (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
    (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
    (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
    (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

(use-package company-tern
  :bind
  ("M-." . nil)
  ("M-," . nil)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-tern-property-marker " <p>"))

(use-package indium
  :config
  (add-hook 'indium-update-script-source-hook
            (lambda (url)
              (indium-eval (format "window.dispatchEvent(new CustomEvent('patch', {detail: {url: '%s'}}))"
                                   url))))
  (indium-interaction-mode))

(use-package js2-mode
  :after indium
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
  :bind
  (:map js2-mode-map
        ("C-k" . js2r-kill))
  :config
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (js2r-add-keybindings-with-prefix "C-c C-r"))

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

(use-package tagedit)

(use-package sgml-mode
  :after tagedit
  :config
  (require 'tagedit)
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'html-mode-hook (lambda () (tagedit-mode 1))))

(use-package yard-mode
  :hook (ruby-mode . yard-mode))

(use-package rbenv
  :hook (ruby-mode . global-rbenv-mode)
  :config
   (setq rbenv-installation-dir "/usr/local/bin/rbenv"))

(use-package robe
  :hook (ruby-mode . robe-mode))

(use-package inf-ruby
  :bind
  (:map inf-ruby-minor-mode-map
        (("C-c C-z" . run-ruby)
         ("C-c C-b" . ruby-send-buffer)))
  :config
  (progn
      (when (executable-find "pry")
        (add-to-list 'inf-ruby-implementations '("pry" . "pry"))
        (setq inf-ruby-default-implementation "pry"))))

(use-package ruby-mode
  :after robe
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby"
  :init
  (progn
    (setq ruby-indent-level 2
          ruby-indent-tabs-mode nil)
    (add-hook 'ruby-mode 'superword-mode)
    (robe-start)))
      ;;(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)))

(use-package flycheck-rust
  :commands (flycheck-rust-setup))

(use-package lsp-rust
  :hook (rust-mode . lsp-rust-enable)
  :init
  (progn
     (setq RUSTC "~/.cargo/bin/rustc")
     (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))))

(use-package rust-mode
  :mode "\\.rs\\'"
  :bind
  (:map rust-mode-map
        (([tab] . company-indent-or-complete-common)
         ("C-c <tab>" . rust-format-buffer)))
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'company-mode)
  ;; (add-hook 'rust-mode-hook 'racer-mode)
  ;; (add-hook 'rust-mode-hook 'ycmd-mode)
  (add-hook 'rust-mode-hook 'flycheck-rust-setup)
  :config
  (electric-pair-mode 1)
  (lsp-mode)
  ;(lsp-rust-enable)
  )

(use-package json-mode
  :mode "\\.json\\'")

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(use-package docker
  :config
  (progn
    (setenv "DOCKER_TLS_VERIFY" "1")
    (setenv "DOCKER_HOST" "tcp://10.11.12.13:2376")
    (setenv "DOCKER_CERT_PATH" "/Users/justin/.docker/machine/machines/box")
    (setenv "DOCKER_MACHINE_NAME" "box")))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package flymd
 :commands (flymd-flyit))

(use-package lua-mode
  :mode ("\\.lua\\'")
  :config
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(use-package terraform-mode
:mode "\\.tf\\'" )

(use-package slack
  :commands (slack-start slack-register-team)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "personal"
   :default t
   :client-id (getenv "SLACK_CLIENT_ID")
   :client-secret (getenv "SLACK_CLIENT_SECRET")
   :token (getenv "SLACK_TOKEN")
   :subscribed-channels '(general)))

  ;; (slack-register-team
  ;;  :name "test"
  ;;  :client-id "3333333333.77777777777"
  ;;  :client-secret "cccccccccccccccccccccccccccccccc"
  ;;  :token "xxxx-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
  ;;  :subscribed-channels '(hoge fuga)))

  (use-package alert
    :commands (alert)
    :init
    (setq alert-default-style 'notifier))

(use-package flx)

(use-package woman
  :ensure nil
  :config
  (progn (setq woman-manpath
              (split-string (shell-command-to-string "man --path") ":" t "\n"))
        (autoload 'woman "woman"
          "Decode and browse a UN*X man page." t)
        (autoload 'woman-find-file "woman"
          "Find, decode and browse a specific UN*X man-page file." t)))

(use-package ido-completing-read+)

(use-package deferred)

(use-package esup
  :commands (esup))

(use-package profiler
  :bind
  (("s-l" . profiler-start)
   ("s-r" . profiler-report)))

(use-package dired+)

(defun font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
The pixel size of the frame is kept (approximately) the same.
DELTA should be a multiple of 10, in the units used by the
:height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        ;; Latest 'set-frame-font supports a "frames" arg, but
        ;; we cater to Emacs 23 by looping instead.
        (set-frame-font (font-name-replace-size (face-font 'default)
                                                new-point-height)
                        t)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun increase-default-font-height ()
  (interactive)
  (increment-default-font-height 10))

(defun decrease-default-font-height ()
  (interactive)
  (increment-default-font-height -10))

(global-set-key (kbd "C-M-=") 'increase-default-font-height)
(global-set-key (kbd "C-M--") 'decrease-default-font-height)

(defun scroll-up-one-line-other-window ()
  "Scroll other window one line up"
  (interactive)
  (scroll-other-window 1))

(defun scroll-down-one-line-other-window ()
  "Scroll other window one line down"
  (interactive)
  (scroll-other-window -1))

(defun my/tangle-dotfiles ()
  "If the current file is this file, the code blocks are tangled"
  (when (equal (buffer-file-name) (expand-file-name "~/.emacs.d/README.org"))
    (org-babel-tangle nil "~/.emacs.d/init.el")))
    ;;(byte-compile-file "~/.emacs.d/init.el")
(add-hook 'after-save-hook #'my/tangle-dotfiles)

(defun xah-run-current-file ()
      "Execute the current file.
    For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
    The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
    File suffix is used to determine what program to run.

    If the file is modified or not saved, save it automatically before run.

    URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
    version 2016-01-28"
      (interactive)
      (let* ((-suffix-map
             ;; (‹extension› . ‹shell program name›)
             `(("php" . "php")
               ("pl" . "perl")
               ("py" . "python")
               ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
               ("rb" . "ruby")
               ("go" . "go run")
               ("js" . "node") ; node.js
               ("sh" . "bash")
               ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
               ("rkt" . "racket")
               ("ml" . "ocaml")
               ("vbs" . "cscript")
               ("tex" . "pdflatex")
               ("latex" . "pdflatex")
               ("java" . "javac")))
            -fname
            -fSuffix
            -prog-name
            -cmd-str)

        (when (null (buffer-file-name)) (save-buffer))
        (when (buffer-modified-p) (save-buffer))

        (setq -fname (buffer-file-name))
        (setq -fSuffix (file-name-extension -fname))
        (setq -prog-name (cdr (assoc -fSuffix -suffix-map)))
        (setq -cmd-str (concat -prog-name " \""   -fname "\""))

        (cond
         ((string-equal -fSuffix "el") (load -fname))
         ((string-equal -fSuffix "java")
          (progn
            (shell-command -cmd-str "*xah-run-current-file output*" )
            (shell-command
             (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname))))))
         (t (if -prog-name
                (progn
                  (message "Running…")
                  (shell-command -cmd-str "*xah-run-current-file output*" ))
              (message "No recognized program file suffix for this file."))))))
  ;;  (global-set-key (kbd "s-r") 'xah-run-current-file)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-unset-key (kbd "C-x C-e"))
(global-set-key (kbd "C-x C-e") 'eval-and-replace)

;; use 4 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(defun js2-unused--find-definitions ()
  ;; Reset the value before visiting the AST
  (setq js2-unused-definitions nil)
  (js2-visit-ast js2-mode-ast
                 #'js2-unused-visitor))
(defun js2-unused--unqualified-name (name)
  "Return the local name of NAME.
foo.bar.baz => baz"
  (save-match-data
    (if (string-match "\\.\\([^.]+\\)$" name)
        (match-string 1 name)
      name)))

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
      (if-let* ((name (js2-function-name node)))
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

(defun enable-paredit ()
    (turn-off-smartparens-mode)
    (paredit-mode))

  (defun enable-parinfer ()
    (turn-off-smartparens-mode)
    (parinfer-mode))

(defun enable-lispy ()
    (turn-off-smartparens-mode)
    (lispy-mode))

(defun jb/slack-quote-region ()
    (with-temp-buffer
      (insert region)
      (goto-char 1)
      (while (> (point-max) (point))
        (beginning-of-line)
        (insert "> ")
        (forward-line 1))
      (buffer-string)))

(defun jb/decorate-text (text)
  (let* ((decorators '(("None" . (lambda (text) text))
                       ("Code"  . (lambda (text) (concat "```" text "```")))
                       ("Quote"  . (lambda (text) (jb/slack-quote-region text)))))
         (decoration (completing-read "Select decoration: "
                                      decorators
                                      nil
                                      t)))
    (funcall (cdr (assoc decoration decorators)) text)))

(defun jb/send-region-to-slack ()
  (interactive)
  (let* ((team (slack-team-select))
         (room (slack-room-select
                (cl-loop for team in (list team)
                         append (with-slots (groups ims channels) team
                                  (append ims groups channels))))))
    (slack-message-send-internal (jb/decorate-text (filter-buffer-substring
                                                    (region-beginning) (region-end)))
                                 (oref room id)
                                 team)))

(setq file-name-handler-alist doom--file-name-handler-alist)
