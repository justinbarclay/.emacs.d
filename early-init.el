;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold 1000000000)
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq-default gc-cons-threshold (* 1024 1024 200))
   (message "gc-cons-threshold restored to %S"
            gc-cons-threshold)))

(when (featurep 'jsonrpc)
    (unload-feature 'jsonrpc))

(setq custom-file (expand-file-name "customs.el" user-emacs-directory))

(add-hook 'after-init-hook (lambda () (load custom-file 'noerror)))

(setq package-enable-at-startup nil)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(setq load-prefer-newer t)
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(defun my/tangle-dotfiles ()
  "If the current file is this file, the code blocks are tangled"
  (interactive)
  (when (equal (buffer-file-name)
   (expand-file-name "~/.emacs.d/README.org"))
    (org-babel-tangle '() (expand-file-name "~/.emacs.d/init.el"))))
    ;;(byte-compile-file "~/.emacs.d/init.el")
(add-hook 'after-save-hook #'my/tangle-dotfiles)

(defvar jb/os-linux-p (eq system-type 'gnu/linux))
(defvar jb/os-windows-p (eq system-type 'windows-nt))
(defvar jb/os-macos-p (eq system-type 'darwin))

(when jb/os-macos-p
  (setq default-frame-alist '((ns-appearance . dark) (ns-transparent-titlebar . t) (ns-appearance . 'nil))))

(tool-bar-mode -1)

(menu-bar-mode -1)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(pixel-scroll-precision-mode 1)

(when (display-graphic-p) ; Start full screen
  (add-to-list 'default-frame-alist '(fullscreen . t))
  (x-focus-frame nil))

(setq-default frame-title-format "%b (%f)")

(setq frame-resize-pixelwise 't)

(let ((font-name (if jb/os-windows-p
                     "CaskaydiaCove NFM"
                   "CaskaydiaMono Nerd Font Mono")))
  (set-face-attribute 'default nil
                      :family font-name :height 160 :weight 'normal))

;; These settings relate to how emacs interacts with your operating system
(setq-default ;; makes killing/yanking interact with the clipboard
 select-enable-clipboard t

 ;; I'm actually not sure what this does but it's recommended?
 select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 ;;save-interprogram-paste-before-kill nil ;; This is disabled because it crashes emacs.

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

(setq-default ring-bell-function 'ignore)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq-default create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq-default inhibit-startup-message t)

(setq bidi-inhibit-bpa t)

(setq-default read-process-output-max (* 1024 1024)) ;; 1mb

(global-display-line-numbers-mode)
(set-default 'display-line-numbers-type 't)
(set-default 'display-line-numbers-current-absolute 't)

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
