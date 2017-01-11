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
    flycheck-ycmd

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


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))


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
  ;;
  ;; Add a directory to our load path so that when you `load` things
  ;; below, Emacs knows where to look for the corresponding file.

(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; ;; These customizations change the way emacs looks and disable/enable
;; ;; some user interface elements
(load "ui.el")

;; ;; These customizations make editing a bit nicer.
(load "editing.el")

;; ;; Hard-to-categorize customizations
(load "misc.el")

;; ;; Setup for auto complete
(load "company-config.el")
;; ;; For editing lisps
(load "elisp-editing.el")
;; ;; Auto-complete backend for python, javascript, C, C++, GO, Rust
(load "ycmd-config.el")

;; ;; Langauage-specific
(load "setup-c++.el")
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-python.el")
(load "setup-ruby.el")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   (quote
    (minimap exec-path-from-shell indent-guide yard-mode use-package tagedit smex smartparens robe rainbow-delimiters projectile paredit multiple-cursors magit ido-ubiquitous gradle-mode fontawesome flycheck-ycmd fill-column-indicator esup eclim company-ycmd company-quickhelp clojure-mode-extra-font-locking cider ample-theme airline-themes))))
