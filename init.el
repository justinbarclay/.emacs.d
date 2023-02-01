(setq comp-deferred-compilation-deny-list '())
(setq native-comp-async-report-warnings-errors nil)

(setq
 lexical-binding t
 load-prefer-newer t)

(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq user-full-name "Justin Barclay"
      user-mail-address "me@justincbarclay.ca")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-compute-statistics t)
(setq use-package-minimum-reported-time 0.01)

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)
(setq use-package-always-defer t)
(setq use-package-enable-imenu-support t)

(use-package diminish)                ;; if you use :diminish
(use-package bind-key)                ;; if you use any :bind variant

(use-package gnu-elpa-keyring-update)

(use-package org
  :ensure nil
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :init
  (progn
    (setq org-src-tab-acts-natively nil)
    (global-unset-key "\C-c\C-v\C-c")
    (defun jb/org-narrow-to-parent ()
      "Narrow buffer to the current subtree."
      (interactive)
      (widen)
      (org-up-element)
      (save-excursion
        (save-match-data
          (org-with-limited-levels
           (narrow-to-region
            (progn
              (org-back-to-heading t) (point))
            (progn (org-end-of-subtree t t)
                   (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
                   (point)))))))
    (defun jb/org-clear-results ()
      (interactive)
      (org-babel-remove-result-one-or-many 't))
    (defun run-org-block ()
      (interactive)
      (save-excursion
        (goto-char
         (org-babel-find-named-block
          (completing-read "Code Block: " (org-babel-src-block-names))))
        (org-babel-execute-src-block-maybe)))
    (setq global-company-modes '(not org-mode)))
  :config
  (progn
    (setq truncate-lines nil
          org-startup-truncated nil
          word-wrap t)      
    (setq org-agenda-files (list (concat org-directory "/personal/calendar.org")
                                 (concat org-directory "/work/calendar.org")
                                 (concat org-directory "/personal/tasks.org")
                                 (concat org-directory "/work/tasks.org")))
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((shell . t)
                                   (dot . t)
                                   (js . t)
                                   (sql . t)
                                   (python . t)
                                   (ruby . t)))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "INPROGRESS(i)" "|" "DONE(d)")
            ("WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))

          org-todo-keyword-faces
          '(("TODO" :foreground "red" :weight regular)
            ("INPROGRESS" :foreground "blue" :weight regular)
            ("DONE" :foreground "forest green" :weight regular)
            ("WAITING" :foreground "orange" :weight regular)
            ("BLOCKED" :foreground "magenta" :weight regular)
            ("CANCELLED" :foreground "forest green" :weight regular))
          )
    (setq org-log-into-drawer t)

    (setq org-default-notes-file (concat org-directory "/notes.org")
          org-export-html-postamble nil
          org-hide-leading-stars t
          org-startup-folded 'overview
          org-startup-indented t)))

(use-package org-contrib
:after org)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package ob-restclient
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package toc-org
  :hook (org-mode-mode . toc-org-mode))

(use-package org-re-reveal)

(use-package org-tree-slide
  :config
  (progn
    (org-tree-slide-presentation-profile)
    (setq org-tree-slide-slide-in-effect nil
          org-tree-slide-skip-done nil
          org-tree-slide-header nil)))

;; generated-curl-command is used to communicate state across several function calls
(setq generated-curl-command nil)

(defvar org-babel-default-header-args:restclient-curl
  `((:results . "raw"))
  "Default arguments for evaluating a restclient block.")

;; Lambda function reified to a named function, stolen from restclient
(defun gen-restclient-curl-command (method url headers entity)
  (let ((header-args
         (apply 'append
                (mapcar (lambda (header)
                          (list "-H" (format "%s: %s" (car header) (cdr header))))
                        headers))))
    (setq generated-curl-command
          (concat
           "#+BEGIN_SRC sh\n"
           "curl "
           (mapconcat 'shell-quote-argument
                      (append '("-i")
                              header-args
                              (list (concat "-X" method))
                              (list url)
                              (when (> (string-width entity) 0)
                                (list "-d" entity)))
                      " ")
           "\n#+END_SRC"))))

(defun org-babel-execute:restclient-curl (body params)
  "Execute a block of Restclient code to generate a curl command with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Restclient source code block")
  (with-temp-buffer
    (let ((results-buffer (current-buffer))
          (restclient-same-buffer-response t)
          (restclient-same-buffer-response-name (buffer-name))
          (display-buffer-alist
           (cons
            '("\\*temp\\*" display-buffer-no-window (allow-no-window . t))
            display-buffer-alist)))

      (insert (buffer-name))
      (with-temp-buffer
        (dolist (p params)
          (let ((key (car p))
                (value (cdr p)))
            (when (eql key :var)
              (insert (format ":%s = %s\n" (car value) (cdr value))))))
        (insert body)
        (goto-char (point-min))
        (delete-trailing-whitespace)
        (goto-char (point-min))
        (restclient-http-parse-current-and-do 'gen-restclient-curl-command))
      generated-curl-command)))

;; Make it easy to interactively generate curl commands
(defun jb/gen-curl-command ()
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if (equalp "restclient" (car info))
        (org-babel-execute-src-block t (cons "restclient-curl"
                                             (cdr info)))
        (message "I'm sorry, I can only generate curl commands for a restclient block."))))

(use-package org-agenda
  :ensure nil
  :init
  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))
  (setq initial-buffer-choice (lambda () (org-agenda nil "d")
                                (buffer-find "*Org Agenda*")))
  (setq org-agenda-window-setup 'only-window
        org-agenda-custom-commands
        '(("d" "Today"
           ((tags-todo "SCHEDULED<\"<+1d>\"&PRIORITY=\"A\"" ;Priority tasks available to do today
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-span 'day)
                        (org-scheduled-delay-days -14)
                        (org-agenda-overriding-header "Schedule")))
            (tags-todo "SCHEDULED<\"<+1d>\"" ;All tasks available today
                       ((org-agenda-skip-function
                         '(or (org-agenda-skip-entry-if 'done)
                              (air-org-skip-subtree-if-priority ?A)))
                        (org-agenda-overriding-header "Tasks:"))))))))

(use-package elegant-agenda-mode  
  :hook '(org-agenda-mode . elegant-agenda-mode))

(use-package org-alert)

(use-package doct
  :commands (doct)
  :init (setq org-capture-templates
              (doct '(("Personal" :keys "p" :children
                       (("Todo"   :keys "t"
                         :template ("* TODO %^{Description}"
                                    "SCHEDULED: %U")
                         :headline "Tasks" :file "~/org/personal/tasks.org")
                        ("Notes"  :keys "n"
                         :template ("* %^{Description}"
                                    ":PROPERTIES:"
                                    ":Created: %U"
                                    ":END:")
                         :headline "Notes" :file "~/org/personal/tasks.org")
                        ("Appointment"  :keys "a"
                         :template ("* %^{Description}"
                                    "SCHEDULED: %T"
                                    ":PROPERTIES:"                                    
                                    ":calendar-id: justincbarclay@gmail.com"
                                    ":END:")
                         :file "~/org/personal/calendar.org")
                        ("Emails" :keys "e"
                         :template "* TODO [#A] Reply: %a :@home:"
                         :headline "Emails" :file "~/org/personal/tasks.org")))

                      ("Work"    :keys "w"
                       :children
                       (("Todo"  :keys "t"
                         :template ("* TODO %^{Description}"
                                    ":PROPERTIES:"
                                    ":Scheduled: %U"
                                    ":END:")
                         :headline "Tasks" :file "~/org/work/tasks.org")
                        ("Notes"  :keys "n"
                         :template ("* %^{Description}"
                                    ":PROPERTIES:"
                                    ":Created: %U"
                                    ":END:")
                         :headline "Notes" :file "~/org/work/tasks.org")
                        ("Emails" :keys "e"
                         :template "* TODO [#A] Reply: %a :@work:"
                         :headline "Emails" :file "~/org/work/tasks.org")
                        ("Trello" :keys "r"
                         :template ("* TODO [#B] %a " "SCHEDULED: %U")
                         :headline "Tasks" :file "~/org/work/tasks.org")
                        ("Appointment"  :keys "a"
                         :template ("* %^{Description}"
                                    "SCHEDULED: %T"
                                    ":PROPERTIES:"                                      
                                    ":calendar-id: justin.barclay@tidalmigrations.com"
                                    ":END:")
                         :file "~/org/work/calendar.org")))))))

(use-package org-gcal
  :init
  (setq org-gcal-client-id (getenv "CALENDAR_CLIENT_ID")
        org-gcal-client-secret (getenv "CALENDAR_CLIENT_SECRET")
        org-gcal-file-alist '(("justincbarclay@gmail.com" . "~/org/personal/calendar.org")
                              ("justin.barclay@tidalmigrations.com" . "~/org/work/calendar.org"))))

(use-package org-fancy-priorities
  :ensure t
  :hook 
  (org-mode . org-fancy-priorities-mode)
  :config
  '((?A :foreground "red" )
    (?B :foreground "orange")
    (?C :foreground "blue"))
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(use-package flycheck-grammarly)

(use-package langtool
  :init
  (setq langtool-default-language "en-US")
  (setq langtool-bin "/usr/sbin/languagetool"))

(use-package eshell
  :ensure nil
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
  ;; :ensure-system-package git
  :bind (("C-c g" . magit-status))
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

    ;; Close popup when commiting - thios stops the commit window
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

    ;; Customizing transients
    ;; This gives us the option to override local branch
    (transient-insert-suffix 'magit-pull "-r" '("-f" "Overwrite local branch" "--force"))
    ;; magit settings
    (setq
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
     magit-set-upstream-on-push 'askifnotset))
  )

(use-package forge
  :after magit
  :init
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(use-package mu4e
  :ensure nil
  ;; :ensure-system-package mu
  ;; :ensure-system-package (mbsync . isync)
  :commands (mu4e)
  :functions (mu4e--server-filter)
  :hook (mu4e-headers-mode . mu4e-thread-folding-mode)
  :bind (:map mu4e-headers-mode-map
              ("q" . kill-current-buffer))
  :config 
  (setq mu4e-headers-skip-duplicates  t
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-compose-format-flowed nil
   mu4e-date-format "%y/%m/%d"
   mu4e-headers-date-format "%Y/%m/%d"
   mu4e-change-filenames-when-moving t
   mu4e-attachments-dir "~/Downloads"
   mu4e-use-fancy-chars 't
   mu4e-maildir       "~/Maildir"   ;; top-level Maildir
   ;; note that these folders below must start with /
   ;; the paths are relative to maildir root
   mu4e-refile-folder "/Archive"
   mu4e-sent-folder   "/Sent"
   mu4e-drafts-folder "/Drafts"
   mu4e-trash-folder  "/Trash"

   ;; this setting allows to re-sync and re-index mail
   ;; by pressing U
   mu4e-get-mail-command  "mbsync -a"

   mu4e-completing-read-function 'completing-read
   message-send-mail-function   'smtpmail-send-it
   smtpmail-default-smtp-server "smtp.fastmail.com"
   smtpmail-smtp-server         "smtp.fastmail.com")

  (display-line-numbers-mode -1))

(use-package mu4e-dashboard
  :straight (:type git :host github :repo "rougier/mu4e-dashboard")
  :bind ("C-c d" . mu4e-dashboard)
  :hook
  (mu4e-dashboard-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (mu4e-dashboard-file "~/.emacs.d/dashboards/mu4e-dashboard.org")
  :config
  (display-line-numbers-mode -1)
  (flyspell-mode -1))

(use-package mu4e-thread-folding
  :after mu4e
  :straight (:type git :host github :repo "rougier/mu4e-thread-folding")
  :init
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                           :shortname ""
                           :function (lambda (msg) "  "))))
  :custom
  (mu4e-headers-fields '((:empty         .    2)
                         (:human-date    .   12)
                         (:flags         .    6)
                         (:mailing-list  .   10)
                         (:from          .   22)
                         (:subject       .   nil)))
  :bind (:map mu4e-headers-mode-map
              ("<tab>"     . mu4e-headers-toggle-at-point)
              ("<left>"    . mu4e-headers-fold-at-point)
              ("<S-left>"  . mu4e-headers-fold-all)
              ("<right>"   . mu4e-headers-unfold-at-point)
              ("<S-right>" . mu4e-headers-unfold-all)))

(use-package ligature
  :defer t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  :init
  (global-ligature-mode t))

(use-package unicode-fonts
   :defer 't
   :config
   (unicode-fonts-setup))

(cond
 (jb/os-macos-p
  (progn
    (set-fontset-font "fontset-default" 'symbol "Apple Color Emoji" nil 'prepend)
    (set-fontset-font "fontset-default" 'emoji "Apple Color Emoji" nil 'prepend)))
 ((or jb/os-linux-p
      jb/os-windows-p)
  (progn
    (set-fontset-font "fontset-default" 'symbol "Segoe UI Emoji" nil 'prepend)
    (set-fontset-font "fontset-default" 'emoji "Segoe UI Emoji" nil 'prepend)))
 nil)

(blink-cursor-mode 0)

(keymap-global-unset "C-l")

(setq read-process-output-max (* 1024 1024))

(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme"))

(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes") 
  :custom
  (lambda-themes-set-italic-comments nil)
  (lambda-themes-set-italic-keywords nil)
  (lambda-themes-set-variable-pitch nil) 
  :config
  ;; load preferred theme 
  (load-theme 'lambda-light))

(use-package doom-everblush-theme
  :straight (doom-everblush-theme :type git :host github :repo "Everblush/doomemacs"))

(use-package doom-themes
  :defer 1
  :init
  (load-theme 'doom-everblush t)
  ;; When using doom-themes region is a much better colour for highlighting current line
  (defface custom-hl-line
    '((t (:inherit region :background "#2d2844")))
    "Customized HL line face")
  (setq hl-line-face 'custom-hl-line))

(use-package all-the-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)

  (custom-set-faces '(doom-modeline-eyebrowse ((t (:background "#cb619e" :inherit 'mode-line))))
                    '(doom-modeline-inactive-bar ((t (:background "#cb619e" :inherit 'mode-line))))))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  ;; Go back home? Just press `bh'
  (dirvish-bookmark-entries
   '(("h" "~/" "Home")
     ("m" "~/dev/tidal/application-inventory/" "MMP")
     ("t" "~/dev/tidal/tidal-wave" "Tidal Wave")))
  ;; (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format ; it's ok to place string inside
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  ;; Don't worry, Dirvish is still performant even you enable all these attributes
  (dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  ;; Maybe the icons are too big to your eyes
  ;; (dirvish-all-the-icons-height 0.8)
  ;; In case you want the details at startup like `dired'
  ;; (dirvish-hide-details nil)
  :config
  (dirvish-peek-mode)
  ;; Dired options are respected except a few exceptions, see *In relation to Dired* section above
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  ;; Enable mouse drag-and-drop files to other applications
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  ;; Make sure to use the long name of flags when exists
  ;; eg. use "--almost-all" instead of "-A"
  ;; Otherwise some commands won't work properly
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  :bind
  ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   ;; Dirvish has all the keybindings (except `dired-summary') in `dired-mode-map' already
   :map dirvish-mode-map
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dired-up-directory)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("TAB" . dirvish-subtree-toggle)
   ("M-n" . dirvish-history-go-forward)
   ("M-p" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(use-package ibuffer
  :ensure nil
  :defines all-the-icons-icon-alist
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-match-to-alist
              all-the-icons-faicon)
  :commands (ibuffer-current-buffer
             ibuffer-find-file
             ibuffer-do-sort-by-alphabetic)
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))

  ;; Display buffer icons on GUI
  (when (display-graphic-p)
    (define-ibuffer-column icon (:name " ")
      (let ((icon (if (and buffer-file-name
                           (all-the-icons-match-to-alist buffer-file-name
                                                         all-the-icons-icon-alist))
                      (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name)
                                                   :height 0.9 :v-adjust -0.05)
                    (all-the-icons-icon-for-mode major-mode :height 0.9 :v-adjust -0.05))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust -0.05))
          icon)))

    (setq ibuffer-formats '((mark modified read-only locked
                                  " " (icon 2 2 :left :elide) (name 18 18 :left :elide)
                                  " " (size 9 -1 :right)
                                  " " (mode 16 16 :left :elide) " " filename-and-process)
                            (mark " " (name 16 -1) " " filename))))
  :config
  (with-eval-after-load 'counsel
    (defalias 'ibuffer-find-file 'counsel-find-file)))

(use-package ibuffer-projectile
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  :config
  (setq ibuffer-projectile-prefix
        (if (display-graphic-p)
            (concat
             (all-the-icons-octicon "file-directory"
                                    :face ibuffer-filter-group-name-face
                                    :v-adjust -0.1
                                    :height 1.1)
             " ")
          "Project: ")))

(use-package diminish
  :demand t
  :config (progn
            ;;            (diminish 'auto-revert-mode)
            ;;            (diminish 'outline-minor-mode)
            ;;            (diminish 'amd-mode)
            (diminish 'js2-refactor-mode)
            (diminish 'tern-mode)))

(when jb/os-windows-p
  (setq package-check-signature nil)
  (require 'gnutls)
  (add-to-list 'gnutls-trustfiles (expand-file-name "~/.cert/cacert.pm"))
  (setq explicit-shell-file-name "c:/windows/system32/bash.exe")
  (setq shell-file-name "bash")
  (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
  (setenv "SHELL" shell-file-name)
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))

(when jb/os-linux-p
  (use-package pinentry
    :defer 10
    :init (pinentry-start)))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode 1)
  (setq recentf-max-menu-items 40))

(use-package projectile
  :defer 1
  :commands
  (projectile-find-file projectile-switch-project)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'auto)
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action #'magit-status)

  (define-key projectile-mode-map (kbd "C-c p") '("projectile" . projectile-command-map))

  (defvar projectile-other-window-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "a") '("find-other-file-other-window" . projectile-find-other-file-other-window))
      (define-key map (kbd "b") '("switch-to-buffer-other-window" . projectile-switch-to-buffer-other-window))
      (define-key map (kbd "C-o") '("display-buffer" . projectile-display-buffer))
      (define-key map (kbd "d") '("find-dir-other-window" . projectile-find-dir-other-window))
      (define-key map (kbd "D") '("dired-other-window" . projectile-dired-other-window))
      (define-key map (kbd "f") '("find-file-other-window" . projectile-find-file-other-window))
      (define-key map (kbd "g") '("find-file-dwim-other-window" . projectile-find-file-dwim-other-window))
      (define-key map (kbd "t") '("find-implementation-or-test-other-window" . projectile-find-implementation-or-test-other-window))
      map))

  (defvar projectile-other-frame-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "a") '("find-other-file-other-frame" . projectile-find-other-file-other-frame))
      (define-key map (kbd "b") '("switch-to-buffer-other-frame" . projectile-switch-to-buffer-other-frame))
      (define-key map (kbd "d") '("find-dir-other-frame" . projectile-find-dir-other-frame))
      (define-key map (kbd "D") '("dired-other-frame" . projectile-dired-other-frame))
      (define-key map (kbd "f") '("find-file-other-frame" . projectile-find-file-other-frame))
      (define-key map (kbd "g") '("find-file-dwim-other-frame" . projectile-find-file-dwim-other-frame))
      (define-key map (kbd "t") '("find-implementation-or-test-other-frame" . projectile-find-implementation-or-test-other-frame))
      map))

  (defvar projectile-search-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "g") '("grep" . projectile-grep))
      (define-key map (kbd "r") '("ripgrep" . projectile-ripgrep))
      (define-key map (kbd "s") '("ag" . projectile-ag))
      (define-key map (kbd "x") '("find-references" . projectile-find-references))
      map))

  (which-key-add-keymap-based-replacements projectile-command-map
    "4" (cons "other-window" projectile-other-window-map)
    "5" (cons "other-frame" projectile-other-frame-map)
    "s" (cons "search" projectile-search-map)))

(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package rg)

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this))
  :commands (mc/mark-next-like-this mc/mark-previous-like-this))

(define-key global-map (kbd "RET") 'newline-and-indent)

(set-default 'truncate-lines t)

(global-hl-line-mode 1)

(show-paren-mode 1)

(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)

(setq c-basic-offset 2)

(save-place-mode 1)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :bind (:map smartparens-mode-map ("M-<backspace>" . 'backward-kill-word)))

(use-package smartparens-config
 :after smartparens
 :ensure nil)

(use-package hungry-delete
  :hook (prog-mode . global-hungry-delete-mode))

(use-package origami
  :bind ("C-<tab>" . origami-recursively-toggle-node)
  :hook (prog-mode . origami-mode))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

(use-package ws-butler
  :commands (ws-butler-mode)
  :hook (prog-mode . ws-butler-mode))

(use-package flycheck-pos-tip)

(use-package flycheck
  :after flycheck-pos-tip
  :demand t
  :ensure nil
  :init
  (flycheck-define-checker less-stylelint
    "A LESS syntax and style checker using stylelint.

See URL `http://stylelint.io/'."
    :command ("stylelint"
              (eval flycheck-stylelint-args)
              (option-flag "--quiet" flycheck-stylelint-quiet)
              (when flycheck-stylelintrc (config-file "--config" flycheck-stylelintrc)))
    :standard-input t
    :error-parser flycheck-parse-stylelint
    :predicate flycheck-buffer-nonempty-p
    :modes (less-css-mode))
  :config
  (progn
    (global-flycheck-mode)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-standard-error-navigation nil)
    (when 'display-graphic-p (selected-frame)
          (eval-after-load 'flycheck
            (flycheck-pos-tip-mode)))))

(use-package flyspell 
  :ensure nil
  :defer 10
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  ;; :config (setq flyspell-issue-message-flag nil)
  )

(use-package dap-mode
 :init
 (require 'dap-lldb)
 (require 'dap-gdb-lldb))

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("<escape>" . #'keyboard-escape-quit))
  :config
  (vertico-multiform-mode)

  ;; Custom candidate transforms
  (defun +completion-category-highlight-files (cand)
    (let ((len (length cand)))
      (when (and (> len 0)
                 (eq (aref cand (1- len)) ?/))
        (add-face-text-property 0 len 'dired-directory 'append cand)))
    cand)

  (defun +completion-category-highlight-commands (cand)
    (let ((len (length cand)))
      (when (and (> len 0)
                 (with-current-buffer (nth 1 (buffer-list)) ; get buffer before minibuffer
                   (or (eq major-mode (intern cand)) ; check major mode
                       (seq-contains-p local-minor-modes (intern cand))
                       (seq-contains-p global-minor-modes (intern cand))))) ; check minor modes
        (add-face-text-property 0 len '(:foreground "red") 'append cand))) ; choose any color or face you like
    cand)

  (defun +completion-category-truncate-files (cand)
    (if-let ((type (get-text-property 0 'multi-category cand))
             ((eq (car-safe type) 'file))
             (response (ivy-rich-switch-buffer-shorten-path cand 30)))
        response
      cand))

  ;; Custom sorters
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  ;; Extend vertico-multiform abilities
  (defvar +vertico-transform-functions nil)
  (defun +vertico-transform (args)
    (dolist (fun (ensure-list +vertico-transform-functions) args)
      (setcar args (funcall fun (car args)))))
  (advice-add #'vertico--format-candidate :filter-args #'+vertico-transform)

  (setq vertico-multiform-commands
        '((describe-symbol (vertico-sort-function . vertico-sort-alpha))))

  (setq vertico-multiform-categories
        '((symbol (vertico-sort-function . vertico-sort-alpha))
          (command (+vertico-transform-functions . +completion-category-highlight-commands))
          (file (vertico-sort-function . sort-directories-first)
                (+vertico-transform-functions . +completion-category-highlight-files))
          (multi-category (+vertico-transform-functions . +completion-category-truncate-files)))))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(defun ivy-rich-switch-buffer-user-buffer-p (buffer)
  "Check whether BUFFER-NAME is a user buffer."
  (let ((buffer-name
         (if (stringp buffer)
             buffer
           (buffer-name buffer))))
    (not (string-match "^\\*" buffer-name))))

(defun ivy-rich--local-values (buffer args)
  (let ((buffer (get-buffer buffer)))
    (if (listp args)
        (mapcar #'(lambda (x) (buffer-local-value x buffer)) args)
      (buffer-local-value args buffer))))

(defun ivy-rich-switch-buffer-indicators (candidate)
  (let* ((buffer (get-buffer candidate))
         (process-p (get-buffer-process buffer)))
    (cl-destructuring-bind
        (filename directory read-only)
        (ivy-rich--local-values candidate '(buffer-file-name default-directory buffer-read-only))
      (let ((modified (if (and (buffer-modified-p buffer)
                               (null process-p)
                               (ivy-rich-switch-buffer-user-buffer-p candidate))
                          "*"
                        ""))
            (readonly (if (and read-only (ivy-rich-switch-buffer-user-buffer-p candidate))
                          "!"
                        ""))
            (process (if process-p
                         "&"
                       ""))
            (remote (if (file-remote-p (or filename directory))
                        "@"
                      "")))
        (format "%s%s%s%s" remote readonly modified process)))))

(defun ivy-rich-switch-buffer-shorten-path (file len)
  "Shorten the path of FILE until the length of FILE <= LEN.
  For example, a path /a/b/c/d/e/f.el will be shortened to
     /a/…/c/d/e/f.el
  or /a/…/d/e/f.el
  or /a/…/e/f.el
  or /a/…/f.el."
  (if (> (length file) len)
      (let ((new-file (replace-regexp-in-string "/?.+?/\\(\\(…/\\)?.+?\\)/.*" "…" file nil nil 1)))
        (if (string= new-file file)
            file
          (ivy-rich-switch-buffer-shorten-path new-file len)))
    file))

(defun +marginalia-buffer-get-directory-name (cand)
  (let ((name (buffer-file-name cand)))
    (if name
        (file-name-directory name)
      (buffer-local-value 'list-buffers-directory cand))))

(defun +marginalia-display-project-name (cand)
  (if-let ((dir (+marginalia-buffer-get-directory-name cand))
           (message dir))
      (projectile-project-name
       (projectile-project-root dir))
    "-"))

(defun +marginalia-category-truncate-files (cand)
  (if-let ((type (get-text-property 0 'multi-category cand))
           ((eq (car-safe type) 'file)))
      (ivy-rich-switch-buffer-shorten-path cand 30)
    cand))

(defun +marginalia-truncate-helper (cand)
  (if-let ((func (alist-get (vertico--metadata-get 'category)
                            +marginalia-truncation-func-overrides))
               (shortened-candidate (funcall func cand)))
      shortened-candidate
    cand))

(use-package marginalia
  :config
  (setq marginalia-max-relative-age 0)
  (setq marginalia-align 'left)
  (defvar +marginalia-truncation-func-overrides
    `((file . ,#'+marginalia-category-truncate-files)
      (multi-category . ,#'+marginalia-category-truncate-files))
    "Alist mapping category to truncate functions.")

  (defun marginalia--align (cands)
  "Align annotations of CANDS according to `marginalia-align'."
  (cl-loop for (cand . ann) in cands do
           (when-let (align (text-property-any 0 (length ann) 'marginalia--align t ann))
             (setq marginalia--cand-width-max
                   (max marginalia--cand-width-max
                        (+ (string-width (+marginalia-truncate-helper cand))
                           (compat-call string-width ann 0 align))))))
  (setq marginalia--cand-width-max (* (ceiling marginalia--cand-width-max
                                               marginalia--cand-width-step)
                                      marginalia--cand-width-step))
  (cl-loop for (cand . ann) in cands collect
           (progn
             (when-let (align (text-property-any 0 (length ann) 'marginalia--align t ann))
               (put-text-property
                align (1+ align) 'display
                `(space :align-to
                        ,(pcase-exhaustive marginalia-align
                           ('center `(+ center ,marginalia-align-offset))
                           ('left `(+ left ,(+ marginalia-align-offset marginalia--cand-width-max 2)))
                           ('right `(+ right ,(+ marginalia-align-offset 1
                                                 (- (compat-call string-width ann 0 align)
                                                    (string-width ann)))))))
                ann))
             (list (+marginalia-truncate-helper cand) "" ann))))

  (defun marginalia-annotate-buffer (cand)
    "Annotate buffer CAND with modification status, file name and major mode."
    (when-let (buffer (get-buffer cand))
      (marginalia--fields
       ((file-size-human-readable (buffer-size buffer)) :face 'marginalia-number :width -10)
       ((ivy-rich-switch-buffer-indicators buffer) :face 'error :width 3)
       ((+marginalia-display-project-name buffer) :face 'success :width 15)
       ((ivy-rich-switch-buffer-shorten-path
         (+marginalia-buffer-get-directory-name
          buffer)
         30)
        :face 'marginalia-file-name))))
  :bind
  (("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package orderless
  :config
  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))
  :custom

  (completion-styles '(orderless basic))      ; Use orderless
  (completion-category-overrides
   '((file (styles basic ; For `tramp' hostname completion with `vertico'
                   partial-completion
                   orderless))))
  (orderless-component-separator 'orderless-escapable-split-on-space)

  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp))

  (orderless-style-dispatchers '(prot-orderless-literal-dispatcher)))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind ;; C-c bindings (mode-specific-map)
  (("C-c h" . consult-history)
  ("C-c m" . consult-mode-command)
  ("C-c k" . consult-kmacro)
  ("C-s" . consult-line)
  ;; C-x bindings (ctl-x-map)
  ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
  ;; Custom M-# bindings for fast register access
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ("C-M-#" . consult-register)
  ;; Other custom bindings
  ("M-y" . consult-yank-pop)                ;; orig. yank-pop
  ("<help> a" . consult-apropos)            ;; orig. apropos-command
  ;; M-g bindings (goto-map)
  ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ;; M-s bindings (search-map)
  ("M-s d" . consult-find)
  ("M-s D" . consult-locate)
  ("M-s r" . consult-ripgrep)

  ("M-s u" . consult-focus-lines))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   projectile-find-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :init
  (global-corfu-mode)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-auto-delay 0.1
        corfu-auto 't
        corfu-auto-prefix 2
        corfu-min-width 40
        corfu-min-height 20)

  ;; You can also enable Corfu more generally for every minibuffer, as
  ;; long as no other completion UI is active. If you use Mct or
  ;; Vertico as your main minibuffer completion UI, the following
  ;; snippet should yield the desired result.
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil) ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (custom-set-faces '(corfu-current ((t :inherit region :background "#2d2844"))))
  (custom-set-faces '(corfu-popupinfo ((t :inherit corfu-default))))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package kind-icon
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :custom
  (kind-icon-default-face 'corfu-default)  ; Have background color be the same as `corfu' face background
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0)))

(use-package yasnippet
  :init
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package yasnippet-capf
  :straight (:type git :host github :repo "justinbarclay/yasnippet-capf"))

(use-package treesit
  :ensure nil
  :config
  (setq-default treesit-font-lock-level 4))

(use-package treesit-auto
  :if (and (require 'treesit)
           (treesit-available-p))
  :init
  (global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt))

(use-package lsp-mode
  :commands lsp
  :hook ((rustic-mode
          rjsx-mode
          web-mode
          ruby-base-mode
          c-mode
          javascript-base-mode
          typescript-base-mode)
         . lsp-deferred)
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-idle-delay 0.1
        lsp-log-io nil
        lsp-completion-provider :none
        lsp-headerline-breadcrumb-enable nil
        lsp-keymap-prefix "C-l")
  :bind-keymap ("C-l" . lsp-command-map)
  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package css-mode
  :ensure nil
  :config
  (setq css-indent-offset 2))

(use-package less-css-mode
  :ensure nil
  :hook (less-css-mode . flycheck-mode)
  :config
  (setq css-indent-offset 2))

(use-package rainbow-mode
  :hook ((css-mode . rainbow-mode)
         (less-mode . rainbow-mode)))

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package c-mode
  :ensure nil
  :config
  (progn ; C mode hook
    (add-hook 'c-mode-hook 'flycheck-mode)))

(use-package c++-mode
    :ensure nil)

(use-package c-eldoc)

(defun enable-paredit ()
    (turn-off-smartparens-mode)
    (paredit-mode))

  (defun enable-parinfer ()
    (global-hungry-delete-mode 0)
    (turn-off-smartparens-mode)
    (paredit-mode)
    (parinfer-rust-mode))

(defun enable-lispy ()
    (turn-off-smartparens-mode)
    (lispy-mode))

(use-package paredit
  :commands (paredit-mode)
  :hook ((common-lisp-mode . (lambda () (enable-paredit)))
         (scheme-mode . (lambda () (enable-paredit)))
         (lisp-mode . (lambda () (enable-paredit)))))

(use-package parinfer-rust-mode
 :defer 10
 :commands (parinfer-rust-mode)
 :init
 (setq parinfer-rust-auto-download nil))

(use-package eldoc
  :ensure nil
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  (global-eldoc-mode))

(use-package elisp-mode
    :ensure nil
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (enable-paredit))))

(use-package flycheck-joker
  :defer t
  :init
  (require 'flycheck-joker))

;; clojure refactor library
;; https://github.com/clojure-emacs/clj-refactor.el
(use-package clj-refactor
  :after clojure-mode
  :config (progn (setq cljr-suppress-middleware-warnings t)
                 (add-hook 'clojure-mode-hook (lambda ()
                                                (clj-refactor-mode 1)
                                                (cljr-add-keybindings-with-prefix "C-c C-m")))))

(use-package kibit-helper
  :defer t)

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
    (font-lock-add-keywords
     nil
     '(("(\\(facts?\\)"
        (2 font-lock-keyword-face))
       ("(\\(background?\\)"
        (2 font-lock-keyword-face))))
    (electric-pair-mode 1)
    (setq define-clojure-indent 2)))

(use-package cider
  :hook ((clojure-mode . cider-mode)
         (clojurescript-mode . cider-mode))
  :commands (cider-jack-in cider-jack-in-clojurescript)
  :config
  (progn
    ;; REPL related stuff
    ;; REPL history file
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

    ;; key bindings
    ;; these help me out with the way I usually develop web apps
    (defun cider-refresh ()
      (interactive)
      (cider-interactive-eval (format "(user/reset)")))))

(defun cider--tooltip-show ()
  (interactive)
  (if-let ((info (cider-var-info (thing-at-point 'symbol))))
      (nrepl-dbind-response info (doc arglists-str name ns)
        (pos-tip-show (format "%s : %s\n%s\n%s" ns name arglists-str doc)
                      nil
                      nil
                      nil
                      -1))
    (message "info not found")))

(use-package subword-mode
  :ensure nil
  :hook typescript-ts-base-mode)

(use-package typescript-ts-base-mode
    :ensure nil
    :custom
    (typescript-indent-level 2)
    (lsp-eslint-enable 't)
    :config
    (flycheck-add-mode 'javascript-eslint 'typescript-ts-base-mode))

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'")

(use-package tsx-ts-mode
  :ensure nil
  :mode "\\.tsx\\'")

(use-package rjsx-mode
  :mode (("\\.jsx\\'" . rjsx-mode)
         ("\\.js\\'" . rjsx-mode))
  :bind (:map js2-mode-map ("M-." . xref-find-definitions))
  :config
  (add-hook 'rjsx-mode #'subword-mode)
  (setq js-indent-level 2)
  (setq js2-basic-offset 2))

(use-package js2-refactor
  :bind
  (:map js2-mode-map
        ("C-k" . js2r-kill))
  :config
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package prettier-js
  :hook ((typescript-ts-base-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

(use-package eslint-disable-rule)

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2 

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        flycheck-javascript-eslint-executable "eslint_d"
        flycheck-typescript-tslint-executable "eslint_d")
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package tagedit
 :defer t)

(use-package sgml-mode
  :ensure nil
  :after tagedit
  :config
  (require 'tagedit)
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'html-mode-hook (lambda () (tagedit-mode 1))))

(use-package rbenv
  :hook (ruby-base-mode . global-rbenv-mode)
  :config
   (setq rbenv-installation-dir "/usr/local/bin/rbenv"))

(use-package inf-ruby
  :defer t
  :bind
  (:map inf-ruby-minor-mode-map
        (("C-c C-z" . run-ruby)
         ("C-c C-b" . ruby-send-buffer)))
  :config
  (progn
      (when (executable-find "pry")
        (add-to-list 'inf-ruby-implementations '("pry" . "pry"))
        (setq inf-ruby-default-implementation "pry"))))

(use-package ruby-ts-mode
  :ensure nil
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
    (add-hook 'ruby-ts-mode 'superword-mode)))

(use-package rubocopfmt
:hook (ruby-base-mode . rubocopfmt-mode))

(use-package rspec-mode
 :hook (ruby-base-mode . rspec-enable-appropriate-mode)
 :config
 (rspec-install-snippets))

(use-package rustic
  :bind ("C-c r" . rustic-compile)
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (progn
    (setq rustic-lsp-server 'rust-analyzer)
    (setq rustic-format-on-save nil)
    (setq rustic-indent-offset 2)
    (electric-pair-mode 1)))

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (setq js-indent-level 2))

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :config
  (setq-local tab-width 2))

(use-package docker
  :defer t)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package flymd
 :commands (flymd-flyit))

(use-package powershell
  :mode "\\.ps\\'")

(use-package terraform-mode
:mode "\\.tf\\'" )

(use-package yaml-mode
  :defer t)

(use-package ssh-config-mode
  :defer t)

;; (use-package sqlint)

(use-package rst
    :ensure nil
  :mode (("\\.txt$" . rst-mode)
         ("\\.rst$" . rst-mode)
         ("\\.rest$" . rst-mode)))

(use-package alert
      :commands (alert alert-define-style)
      :config
      (defun alert-burnt-toast-notify (info)
        (let ((args
               (list
                "-c" "New-BurntToastNotification"
                "-Text" (if-let ((title (plist-get info :title)))
                            (format "'%s', '%s'" title (plist-get info :message))
                          (format "'%s'" (plist-get info :message)))
                )))
          (apply #'start-process (append '("burnt-toast" nil "powershell.exe") args))))
      (alert-define-style 'burnt-toast :title "Notify Windows 10 using the PowerShell library BurntToast"
                          :notifier
                          #'alert-burnt-toast-notify)
      (setq alert-default-style 'burnt-toast))

(use-package helpful
  :defer t
  :bind  (("C-h f" . helpful-callable)
          ("C-h v" . helpful-variable)
          ("C-h k" . helpful-key)))

(use-package which-key
  :defer 't
  :init (which-key-mode))

(use-package woman
  :ensure nil
  :config
  (progn (setq woman-manpath
              (split-string (shell-command-to-string "man --path") ":" t "\n"))
        (autoload 'woman "woman"
          "Decode and browse a UN*X man page." t)
        (autoload 'woman-find-file "woman"
          "Find, decode and browse a specific UN*X man-page file." t)))

(use-package esup
  :commands (esup))

(use-package profiler
  :ensure nil
  :bind
  (("s-l" . profiler-start)
   ("s-r" . profiler-report)))

(use-package restclient)

(defmacro comment (docstring &rest body)
  "Ignores body and yields nil"
  nil)

(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.d/README.org"))

(global-set-key (kbd "C-c i") 'open-config-file)

;; use 4 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun jb/mean (a)
  (/ (apply '+ a)
     (length a)))
(defun jb/square (a)
  (* a a))

(defun jb/stdev (a)
  (sqrt
   (/
    (apply '+ (mapcar 'square (mapcar (lambda (subtract)
                                        (- subtract (mean a)))
                                      a)))
    (- (length a) 1 ))))

(defun take-screenshot ()
  (interactive)
  (let ((frame-height (read-number "Enter frame height: " 40))
        (frame-width (read-number "Enter frame width: " 55))
        (filename (read-file-name "Where would you like to save the svg? ")))
    (set-frame-width (selected-frame) frame-width)
    (set-frame-height (selected-frame) frame-height)
    (with-temp-file filename
      (insert (x-export-frames nil 'svg)))
    (kill-new filename)
    (message filename)))

(setq file-name-handler-alist doom--file-name-handler-alist)

(defun count-repititions ()
  (interactive)
  ;; 
  (let ((tracker (make-hash-table :test 'equal))
        (buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer buffer)
      (goto-char (point-min))
      (replace-regexp "^[0-9]+:[0-9][0-9]" "")        
      (delete-blank-lines)
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (delete-blank-lines)
        (let ((current-line (string-trim
                             (buffer-substring-no-properties ;; current-line
                              (line-beginning-position)
                              (line-end-position)))))
          (when (string-match "^All measurement" current-line)
            (puthash current-line
                     (+ 1 (gethash current-line tracker 0))
                     tracker)))
        (forward-line 1)))
    (message "%s" (length (hash-table-keys tracker)))
    (with-current-buffer (get-buffer-create "*repititions*")
      (erase-buffer)
      (maphash (lambda (k v)
                 (insert (format "%s - %s\n" v k)))
               tracker)
      (goto-char (point-min))
      (sort-numeric-fields 1 (point-min) (point-max)))))
