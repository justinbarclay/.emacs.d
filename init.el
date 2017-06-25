;;;;
;; Packages
;;;;
;; This sets the garbage collection threshold to 100mb
(setq gc-cons-threshold 100000000)
;; Reset garbage collection to emacs default after 5s
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000)
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

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; Setup for Lisp Development
    slime-company
    
    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ;; ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; multiple cursors
    multiple-cursors

    ;; auto-complete for easy auto-completeing for languages
    company
    company-quickhelp
    ycmd
    company-ycmd

    ;; add syntax checking
    flycheck

    ;; visibile max line length
    fill-column-indicator

    ;; git integration
    magit
    
    ;; Powerline and themes
    powerline
    airline-themes
    ample-theme
    
    ;; Start up profiler
    esup

    ;; Ruby specific packages
    inf-ruby
    robe
    rbenv
    smartparens
    yard-mode

    ;; Swift Packages
    swift-mode
    flycheck-swift
    company-sourcekit
    

    ;; Org-mode
    calfw ;; Support for fancy Calendar
    calfw-gcal ;; Support for Google Calendar
    org-gcal ;; Support to Sync Google Calendar with org agenda
    ))



;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(when (memq system-type '(darwin))
  (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))




;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;

;;Turn off tool-bar
(tool-bar-mode -1)

;; Set C tabs length
(setq-default c-basic-offset 4)
;; Starting conditions for Emacs
;; Setting Emacs to take focus over terminal
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (x-focus-frame nil))

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
;; Load org-mode specific config
(load "setup-org.el")

(use-package gitter
  :ensure t
  :config
  (setq gitter-token "da6457f05d130bb85e7f5499796f378ad5ddc3cb"))

(setq font-lock-maximum-decoration t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "c79c2eadd3721e92e42d2fefc756eef8c7d248f9edefd57c4887fbf68f0a17af" "158013ec40a6e2844dbda340dbabda6e179a53e0aea04a4d383d69c329fba6e6" "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" default)))
 '(fci-rule-color "#383838" t)
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-directory "~/Dropbox/orgfiles")
 '(org-export-html-postamble nil)
 '(org-hide-leading-stars t)
 '(org-startup-folded (quote overview))
 '(org-startup-indented t)
 '(org-startup-truncated t)
 '(package-selected-packages
   (quote
    (ggtags counsel-gtags company-tern tern-context-coloring tern indium rjsx-mode yas-snippet rustfmt c-eldoc dracula-theme cyberpunk-theme yard-mode yaml-mode web-mode use-package undo-tree tagedit swift-mode smex smartparens slime-company robe rbenv rainbow-delimiters racer projectile parinfer paredit org-present org-gcal org-bullets noflet multiple-cursors magit ido-ubiquitous hungry-delete flycheck-ycmd flycheck-swift flycheck-rust flycheck-pos-tip fill-column-indicator exec-path-from-shell esup eclim counsel company-ycmd company-sourcekit company-quickhelp clojure-mode-extra-font-locking cider calfw-gcal calfw avy ample-theme airline-themes))))
