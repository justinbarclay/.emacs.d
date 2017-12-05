;;;;
;; Packages
;;;;
;; This sets the garbage collection threshold to 100mb
(setq gc-cons-threshold 100000000)
;; Reset garbage collection to emacs default after 5s
(run-with-idle-timer
 5 nil
 (lambda ()
 ;;     (setq gc-cons-threshold 10000000)
   (setq gc-cons-threshold 10000000)
   (message "gc-cons-threshold restored to %S"
            gc-cons-threshold)))
(require 'package)
;;(setq package-archives '(("gnu" . "httpss://elpa.gnu.org/packages/")
;;			 ("tromey" . "https://tromey.com/elpa/")
;;			 ("marmalade" . "https://marmalade-repo.org/packages/")
;;			 ("melpa" . "http://melpa.org/packages/")))
;; Define package repositories
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
;;(require 'package)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Set use package options
(progn ;'use-package'
  (require  'use-package)
  (setq use-package-verbose nil)
  ;;(setq use-package-always-defer t)
  (setq use-package-always-ensure t)
  (setq use-package-enable-imenu-support t))

;;;;
;; Customization
;;;;

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH" "RUST_SRC_PATH")))
;;Turn off tool-bar
(tool-bar-mode -1)

;; Starting conditions for Emacs
;; Setting Emacs to take focus over terminal
(when (display-graphic-p) ; Start full screen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (x-focus-frame nil))

;; Semantic is a package that provides language-aware editing commands based on source code parsers. Parsing is a process of analyzing source code based on programming language syntax. Emacs understands your source code through this process to provides features such as contextual code completion, code navigation.
;; Global semantic mode
(use-package semantic 
  :config
  (semantic-mode 1)
  (global-semanticdb-minor-mode 1)
  global-semanti)

(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.

(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; Setup for auto complete
(load "company-config.el")
;; For editing lisps
(load "lisp-editing.el")

;; Auto-complete backend for python, javascript, C, C++, GO, Rust
(load "ycmd-config.el")

;; Langauage-specific
(load "setup-c++.el")
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-python.el")
(load "setup-ruby.el")
(load "setup-swift.el")
(load "setup-c.el")
(load "setup-rust.el")

;; Config and packages for web based files
(load "setup-web.el")
;; Load org-mode specific config
(load "setup-org.el")
;; Load custom functions
(load "functions.el")
;; what does this do?                      
(setq font-lock-maximum-decoration t)
;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#282a36" :foregound "#50fa7b" :inherit (quote mode-line)))))
 '(powerline-active1 ((t (:background "#6272a4" :foregound "#50fa7b" :inherit (quote mode-line)))))
 '(powerline-active2 ((t (:background "#44475a" :foregound "#50fa7b" :inherit (quote mode-line)))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
 '(spaceline-highlight-face ((t (:background "#cb619e" :foreground "#f8f8f2" :inherit (quote mode-line))))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("3fa07dd06f4aff80df2d820084db9ecbc007541ce7f15474f1d956c846a3238f" "af717ca36fe8b44909c984669ee0de8dd8c43df656be67a50a1cf89ee41bde9a" "d21135150e22e58f8c656ec04530872831baebf5a1c3688030d119c114233c24" "66aea5b7326cf4117d63c6694822deeca10a03b98135aaaddb40af99430ea237" "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf" "256a381a0471ad344e1ed33470e4c28b35fb4489a67eb821181e35f080083c36" "cf284fac2a56d242ace50b6d2c438fcc6b4090137f1631e32bedf19495124600" "d61f6c49e5db58533d4543e33203fd1c41a316eddb0b18a44e0ce428da86ef98" "003a9aa9e4acb50001a006cfde61a6c3012d373c4763b48ceb9d523ceba66829" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "0cd56f8cd78d12fc6ead32915e1c4963ba2039890700458c13e12038ec40f6f5" "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "9b1c580339183a8661a84f5864a6c363260c80136bd20ac9f00d7e1d662e936a" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "c79c2eadd3721e92e42d2fefc756eef8c7d248f9edefd57c4887fbf68f0a17af" "158013ec40a6e2844dbda340dbabda6e179a53e0aea04a4d383d69c329fba6e6" "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" default)))
 '(fci-rule-color "#383838" t)
 '(line-number-mode t)
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-directory "~/Dropbox/orgfiles")
 '(org-export-html-postamble nil)
 '(org-hide-leading-stars t)
 '(org-startup-folded (quote overview))
 '(org-startup-indented t)
 '(org-startup-truncated t)
 '(org-trello-current-prefix-keybinding "C-c o")
 '(package-selected-packages
   (quote
    (restclient markdown-mode flymd docker-compose-mode origami folding json-mode ido-completing-read+ md-readme org-trello rebase-mode helm counsel-dash dash-at-point flx robe-mode csharp-mode xref-js2 spaceline ggtags counsel-gtags company-tern tern-context-coloring tern indium rjsx-mode yas-snippet rustfmt c-eldoc dracula-theme cyberpunk-theme yard-mode yaml-mode web-mode use-package undo-tree tagedit swift-mode smex slime-company rbenv rainbow-delimiters racer parinfer paredit org-present org-gcal org-bullets noflet multiple-cursors magit ido-ubiquitous hungry-delete flycheck-ycmd flycheck-swift flycheck-rust flycheck-pos-tip fill-column-indicator exec-path-from-shell esup eclim counsel company-sourcekit company-quickhelp clojure-mode-extra-font-locking cider calfw-gcal calfw avy ample-theme airline-themes)))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(spaceline-info-mode nil)
 '(tool-bar-mode nil)
 '(tramp-syntax (quote default) nil (tramp)))