(setq comp-deferred-compilation-deny-list '())
(setq native-comp-async-report-warnings-errors nil)

(setq
 lexical-binding t
 load-prefer-newer t)

(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq user-full-name "Justin Barclay"
      user-mail-address "justinbarclay@gmail.com")

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

(progn ;'use-package
  (add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
  (require 'use-package)
  (setq use-package-always-ensure t)
  (setq use-package-verbose nil)
  (setq use-package-always-defer t)
  (setq use-package-enable-imenu-support t))


(use-package diminish)                ;; if you use :diminish
(use-package bind-key)                ;; if you use any :bind variant

(use-package gnu-elpa-keyring-update)

(use-package org
  :ensure org-plus-contrib
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c C-v C-c" . jb/org-clear-results))
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

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ob-restclient
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package toc-org
  :hook (org-mode-hook . toc-org-enable))

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

(use-package org-trello
  :commands (org-trello-sync-buffer org-trello-sync-card)
  :init
  (defun org-trello-pull-buffer ()
     "Synchronize current buffer from trello."
     (interactive)
     (org-trello-sync-buffer 'from))
  (defun org-trello-pull-card ()
     "Synchronize card at point from trello."
     (interactive)
     (org-trello-sync-card 'from)))

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

(use-package langtool
  :init
  (setq langtool-default-language "en-US")
  (setq langtool-bin "/usr/sbin/languagetool"))

(use-package eshell
  :ensure nil
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

    ;; Customizing transients
    ;; This gives us the option to override local branch
    (transient-insert-suffix 'magit-pull "-r" '("-f" "Overwrite local branch" "--force"))
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
     magit-set-upstream-on-push 'askifnotset))
  )

(use-package forge
  :after magit
  :init
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(use-package notmuch
  :commands notmuch-poll-async
  :bind (:map notmuch-common-keymap ("G" . notmuch-poll-async-with-refresh))
  :config
  (defun notmuch-new-sentinel (proc msg)
    (let ((buffer (process-buffer proc))
          (status (process-status proc)))
      (when (memq status '(exit signal))
        (kill-buffer buffer)
        (when (eq status 'signal)
          (error "Notmuch: poll script `%s' failed!" notmuch-poll-script))
        (when (eq status 'exit)
          (message "Polling mail...done"))
        (when notmuch-refresh-buffer
          (with-current-buffer notmuch-refresh-buffer
            (notmuch-refresh-this-buffer)
            (setq notmuch-refresh-buffer nil))))))
  (defun notmuch-poll-async-with-refresh ()
    (interactive)
    (setq notmuch-refresh-buffer (current-buffer))
    (notmuch-poll-async))
  (defun notmuch-poll-async ()
    (interactive)
    (message "Polling mail...")
    (notmuch-start-notmuch
     "poll"
     "*notmuch-new*"
     #'notmuch-new-sentinel
     "new"))
  (setq message-sendmail-f-is-evil t
        sendmail-program "msmtp"
        message-sendmail-extra-arguments '("--read-envelope-from")
        send-mail-function 'sendmail-send-it)

  (setq notmuch-saved-searches       
        '((:name "work/inbox" :query "tag:inbox AND to:tidalmigrations.com")
          (:name "work/unread" :query "tag:inbox AND tag:unread AND to:tidalmigrations.com")
          (:name "personal/inbox" :query "tag:inbox AND to:gmail.com")
          (:name "personal/unread" :query "tag:inbox AND tag:unread AND to:gmail.com")
          (:name "personal/unreplied" :query "tag:sent from:justincbarclay@gmail.com date:yesterday.. not thread:\"{to:justincbarclay@gmail.com}\"")
          (:name "personal/replied" :query "tag:sent from:justincbarclay@gmail.com date:yesterday.. thread:\"{to:justincbarclay@gmail.com}\"")
          (:name "personal/recently-sent" :query "tag:sent date:yesterday..")))
  (require 'ol-notmuch))

(cond
 (jb/os-macos-p
  (set-fontset-font "fontset-default" 'symbol "Apple Color Emoji" nil 'prepend))
 ((or jb/os-linux-p
      jb/os-windows-p)
  (set-fontset-font "fontset-default" 'symbol "Segoe UI Emoji" nil 'prepend))
 nil)

(global-set-key (kbd "s-t") '(lambda () (interactive)))

(blink-cursor-mode 0)

(use-package doom-themes 
  :init
  (load-theme 'doom-dracula t))

(use-package all-the-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (progn
    (setq doom-modeline-buffer-file-name-style 'relative-to-project)
    (setq doom-modeline-github nil)
    (custom-set-faces '(doom-modeline-eyebrowse ((t (:background "#cb619e"
                                                                 :inherit 'mode-line))))
                      '(doom-modeline-inactive-bar ((t (:background "#cb619e" :inherit 'mode-line))))
                      '(doom-modeline-bar ((t (:background "#cb619e" :inherit 'mode-line)))))))

(use-package dired
    :ensure nil
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("a" . dired-find-file)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

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
  (progn
    (projectile-global-mode)
    (setq projectile-completion-system 'ivy)
    (setq projectile-enable-caching t)
    (setq projectile-switch-project-action #'magit-status)))

(use-package ivy-rich
  :defines (all-the-icons-icon-alist
            all-the-icons-dir-icon-alist
            bookmark-alist)
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-icon-family
              all-the-icons-match-to-alist
              all-the-icons-faicon
              all-the-icons-octicon
              all-the-icons-dir-is-submodule)
  :preface
  (defun ivy-rich-bookmark-name (candidate)
    (car (assoc candidate bookmark-alist)))

  (defun ivy-rich-buffer-icon (candidate)
    "Display buffer icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((buffer (get-buffer candidate))
             (buffer-file-name (buffer-file-name buffer))
             (major-mode (buffer-local-value 'major-mode buffer))
             (icon (if (and buffer-file-name
                            (all-the-icons-match-to-alist buffer-file-name
                                                          all-the-icons-icon-alist))
                       (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name)
                                                    :height 0.9 :v-adjust -0.05)
                     (all-the-icons-icon-for-mode major-mode :height 0.9 :v-adjust -0.05))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust -0.05))
          icon))))

  (defun ivy-rich-file-icon (candidate)
    "Display file icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((path (concat ivy--directory candidate))
             (file (file-name-nondirectory path))
             (icon (cond ((file-directory-p path)
                          (cond
                           ((and (fboundp 'tramp-tramp-file-p)
                                 (tramp-tramp-file-p default-directory))
                            (all-the-icons-octicon "file-directory" :height 0.93 :v-adjust 0.01))
                           ((file-symlink-p path)
                            (all-the-icons-octicon "file-symlink-directory" :height 0.93 :v-adjust 0.01))
                           ((all-the-icons-dir-is-submodule path)
                            (all-the-icons-octicon "file-submodule" :height 0.93 :v-adjust 0.01))
                           ((file-exists-p (format "%s/.git" path))
                            (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.01))
                           (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
                                (apply (car matcher) (list (cadr matcher) :height 0.93 :v-adjust 0.01))))))
                         ((string-match "^/.*:$" path)
                          (all-the-icons-material "settings_remote" :height 0.9 :v-adjust -0.2))
                         ((not (string-empty-p file))
                          (all-the-icons-icon-for-file file :height 0.9 :v-adjust -0.05)))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust -0.05))
          icon))))
  :hook ((ivy-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)

  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          ivy-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          persp-switch-to-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 50))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-find-file
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer)))
          counsel-file-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-dired
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer)))
          counsel-dired-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-git
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-recentf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type)
            (ivy-rich-bookmark-name (:width 40))
            (ivy-rich-bookmark-info)))
          counsel-projectile-switch-project
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-projectile-find-file
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-file-transformer)))
          counsel-projectile-find-dir
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-dir-transformer)))
          treemacs-projectile
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))))))

(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-initial-inputs-alist nil)
    (counsel-mode)
    (ivy-rich-mode)))

(use-package counsel
  :after ivy
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (setq ivy-initial-inputs-alist nil)
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
  :preface (setq projectile-keymap-prefix (kbd "C-c p"))
  :commands (counsel-projectile-switch-project counsel-projectile-find-file counsel-projectile-find-dir))

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
          treemacs-goto-tag-strategy          'prefetch-index)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (setq treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
          treemacs-icon-fallback (concat
                                  "  "
                                  (all-the-icons-faicon "file-o"
                                                        :face 'all-the-icons-dsilver
                                                        :height 0.9
                                                        :v-adjust -0.05)
                                  " ")
          treemacs-icon-text treemacs-icon-fallback)
    (dolist (item all-the-icons-icon-alist)
      (let* ((extension (car item))
             (func (cadr item))
             (args (append (list (caddr item))
                           '(:height 0.9 :v-adjust -0.05)
                           (cdddr item)))
             (icon (apply func args))
             (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) extension))
             (value (concat "  " icon " ")))
        (ht-set! treemacs-icons-hash (s-replace-regexp "\\?" "" key) value)
        (ht-set! treemacs-icons-hash (s-replace-regexp ".\\?" "" key) value))))
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

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

(use-package ws-butler
  :commands (ws-butler-mode)
  :hook (prog-mode . ws-butler-mode))

(use-package emojify)

(use-package tree-sitter)
(use-package tree-sitter-langs)

(use-package flycheck-pos-tip)

(use-package flycheck
  :after flycheck-pos-tip
  :demand t
  :ensure nil
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

(use-package company-emoji)

(use-package company
  :init
  (global-company-mode t)
  :commands (compant-manual-begin)
  :bind ("C-<tab>" . company-manual-begin)
  :config
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        company-idle-delay 0)
  ;;(add-to-list 'company-backends 'company-emoji)
  )

(use-package lsp-mode
  :commands lsp
  :hook ((rustic-mode . lsp)
         (csharp-mode . lsp)
         (rjsx-mode . lsp)
         (lsp-mode . yas-minor-mode))
  :config
  (setq lsp-idle-delay 0.100
        lsp-log-io nil
        lsp-completion-provider :capf
        lsp-headerline-breadcrumb-enable nil)
  (setq read-process-output-max (* 1024 1024)))

(use-package company-lsp
  :commands company-lsp
  :bind (("M-." . xref-find-definitions)
         ("M-," . xref-pop-marker-stack))
  :after (company lsp)
  ;; :init (push '(company-lsp) company-backends)
  :config
  (setq company-lsp-cache-candidates 'auto)
  (setq company-lsp-async t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package css-mode
  :ensure nil
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
    (add-hook 'c-mode-hook 'flycheck-mode)
    (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
    (eval-after-load 'c-mode '(setq-local eldoc-documentation-function #'ggtags-eldoc-function))
    (setq-default c-basic-offset 2)))

(use-package c++-mode
    :ensure nil
  )

(use-package c-eldoc)

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

    ;; company mode for completion
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode)
    ;; key bindings
    ;; these help me out with the way I usually develop web apps
    (defun cider-refresh ()
      (interactive)
      (cider-interactive-eval (format "(user/reset)")))
    (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
    (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
    (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
    (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

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

(bind-key "C-c t" 'cider--tooltip-show)

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode))

(use-package css-in-js
  :straight (css-in-js :type git :host github :repo "orzechowskid/css-in-js.el"))

(use-package rjsx-mode
  :mode (("\\.jsx\\'" . rjsx-mode)
         ("\\.js\\'" . rjsx-mode))
  :bind (:map js2-mode-map ("M-." . xref-find-definitions))
  :config
  (add-hook 'rjsx-mode #'subword-mode)
  (setq js-indent-level 2)
  (setq js2-basic-offset 2))

(use-package tide
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))      
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package js2-refactor
  :bind
  (:map js2-mode-map
        ("C-k" . js2r-kill))
  :config
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package prettier-js
  :hook ((typescript-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

(use-package csharp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

(use-package sharper
  :bind
  ("C-c n" . sharper-main-transient))

(use-package web-mode
  ;; :after tide
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2 

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))                          
                (setup-tide-mode)))))

(use-package tagedit
 :defer t)

(use-package sgml-mode
  :ensure nil
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
  :commands (robe-start robe-mode)
  :hook (enh-ruby-mode . robe-mode)
  :config
  (push 'company-robe company-backends))

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

(use-package enh-ruby-mode
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
    (add-hook 'enh-ruby-mode 'superword-mode)))

(use-package rubocopfmt
:hook (enh-ruby-mode . rubocopfmt-mode))

(use-package rspec-mode)

(use-package rustic
  :bind ("C-c r" . rustic-compile)
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (progn
    ;; (setq rustic-lsp-setup-p nil)
    (setq rustic-lsp-server 'rust-analyzer)
    (setq rustic-format-on-save nil)
    (setq rustic-indent-offset 2)
    (electric-pair-mode 1)))

(use-package go-mode
  :mode "\\.go\\'"
  :bind (:map go-mode-map
              (("M-." . 'godef-jump)
               ("M-," . 'pop-tag-mark)))
  :config
  (progn
    (add-hook 'go-mode-hook #'lsp-deferred)
    (add-hook 'before-save-hook 'gofmt-before-save)))

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

(use-package lua-mode
  :mode ("\\.lua\\'")
  :config
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

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
          (apply #'start-process (append '("burnt-toast" nil "pwsh.exe") args))))
      (alert-define-style 'burnt-toast :title "Notify Windows 10 using the PowerShell library BurntToast"
                          :notifier
                          #'alert-burnt-toast-notify)
      (setq alert-default-style 'burnt-toast))

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
  :ensure nil
  :bind
  (("s-l" . profiler-start)
   ("s-r" . profiler-report)))

(use-package restclient)

(defmacro comment (docstring &rest body)
  "Ignores body and yields nil"
  nil)

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
               ("fish" . "fish")
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
    (global-hungry-delete-mode 0)
    (turn-off-smartparens-mode)
    (paredit-mode)
    (parinfer-rust-mode))

(defun enable-lispy ()
    (turn-off-smartparens-mode)
    (lispy-mode))

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
                         append (append (slack-team-ims team)
                                        (slack-team-groups team)
                                        (slack-team-channels team)))
                team)))
    (slack-message-send-internal (jb/decorate-text (filter-buffer-substring
                                                    (region-beginning) (region-end)))
                                 room
                                 team)))

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
