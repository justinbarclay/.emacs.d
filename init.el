(setq native-comp-deferred-compilation-deny-list '())
(setq native-comp-async-report-warnings-errors nil)

(setq-default lexical-binding t
              load-prefer-newer t)

(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq-default user-full-name "Justin Barclay"
              user-mail-address "github@justincbarclay.ca")

(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)
(setq use-package-always-defer t)
(setq use-package-enable-imenu-support t)

(setq use-package-compute-statistics t)
(setq use-package-minimum-reported-time 0.01)

(use-package gnu-elpa-keyring-update)

(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
  NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

(use-package org
  :defer 1
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :init
  (global-unset-key "\C-c\C-v\C-c")
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-src-tab-acts-natively nil)
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
  ;; As liberally borrowed from:
  ;; https://github.com/Fuco1/.emacs.d/blob/76e80dd07320b079fa26db3af6096d8d8a4f3bb9/files/org-defs.el#L1863C1-L1922C57
 (defun my-org-archive-file ()
  "Get the archive file for the current org buffer."
  (car (org-archive--compute-location org-archive-location)))

 (defadvice org-archive-subtree (around fix-hierarchy activate)
   (let* ((fix-archive-p (and (not current-prefix-arg)
                              (not (use-region-p))))
          (afile (my-org-archive-file))
          (buffer (or (find-buffer-visiting afile) (find-file-noselect afile)))
          ;; Get all the parents and their tags, we will try to
          ;; recreate the same situation in the archive buffer.
          ;; TODO: make this customizable.
          (parents-and-tags (save-excursion
                              (let (parents)
                                (while (org-up-heading-safe)
                                  (push (list :heading (org-get-heading t t t t)
                                              :tags (org-get-tags nil :local))
                                        parents))
                                parents))))
     ad-do-it
     (when fix-archive-p
       (with-current-buffer buffer
         (goto-char (point-max))
         (while (org-up-heading-safe))
         (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
                ;; TODO: Factor out dash.el
                (path (and olpath
                           (--map
                            (replace-regexp-in-string "^/" "" it)
                            (s-slice-at "/\\sw" olpath))))
                (level 1)
                tree-text)
           (when olpath
             (org-mark-subtree)
             (setq tree-text (buffer-substring (region-beginning) (region-end)))
             (let (this-command) (org-cut-subtree))
             (goto-char (point-min))
             (save-restriction
               (widen)
               (-each path
                 (lambda (heading)
                   (if (re-search-forward
                        (rx-to-string
                         `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                       (progn
                         (org-narrow-to-subtree)
                         (org-set-tags (plist-get (car parents-and-tags) :tags)))
                     (goto-char (point-max))
                     (unless (looking-at "^")
                       (insert "\n"))
                     (insert (make-string level ?*)
                             " "
                             heading)
                     (org-set-tags (plist-get (car parents-and-tags) :tags))
                     (end-of-line)
                     (insert "\n"))
                   (pop parents-and-tags)
                   (cl-incf level)))
               (widen)
               (org-end-of-subtree t t)
               (org-paste-subtree level tree-text))))))))

  (defun run-org-block ()
    (interactive)
    (save-excursion
      (goto-char
       (org-babel-find-named-block
        (completing-read "Code Block: " (org-babel-src-block-names))))
      (org-babel-execute-src-block-maybe)))
  (setq org-directory "~/dev/diary")
  (setq org-agenda-files (list (concat org-directory "/personal/calendar.org")
                               (concat org-directory "/work/calendar.org")
                               (concat org-directory "/personal/tasks.org")
                               (concat org-directory "/work/tasks.org"))
        org-todo-keywords
        '((sequence "TODO(t)" "INPROGRESS(i)" "NEEDSREVIEW(r)"  "|" "DONE(d)")
          ("WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))

        org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight regular)
          ("INPROGRESS" :foreground "blue" :weight regular)
          ("NEEDSREVIEW" :foreground "purple" :weight regular)
          ("DONE" :foreground "forest green" :weight regular)
          ("WAITING" :foreground "orange" :weight regular)
          ("BLOCKED" :foreground "magenta" :weight regular)
          ("CANCELLED" :foreground "forest green" :weight regular))
        org-log-into-drawer 't
        org-startup-truncated nil
        org-default-notes-file (concat org-directory "/notes.org")
        org-export-html-postamble nil
        org-hide-leading-stars 't
        org-startup-folded 'overview
        org-startup-indented 't)
   ;; Add ts language support
  (add-to-list 'org-src-lang-modes '("tsx" . tsx-ts))
  (add-to-list 'org-src-lang-modes '("typescript" . typescript-ts))
  (add-to-list 'org-src-lang-modes '("jsx" . jsx-ts))
  (add-to-list 'org-src-lang-modes '("javascript" . javascript-ts))
  (add-to-list 'org-src-lang-modes '("ruby" . ruby-ts))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
   ;; `org-babel-do-load-languages' significantly slows loading time,
   ;; so let's run this well after we've loaded
  (run-at-time "1 min" nil (lambda ()
                             (org-babel-do-load-languages 'org-babel-load-languages
                                                          '((shell . t)
                                                            (dot . t)
                                                            (js . t)
                                                            (sql . t)
                                                            (python . t)
                                                            (ruby . t))))))

(use-package org-contrib
  :after org)

(use-feature ox-md
  :ensure nil
  :after org)

(use-package ob-restclient
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

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

(use-feature org-agenda
  :config
  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))
  ;; (setq initial-buffer-choice (lambda () (org-agenda nil "d")
  ;;                               (buffer-find "*Org Agenda*")))
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
  :hook (org-agenda-mode . elegant-agenda-mode))

(use-package org-alert)

(use-package doct
  :after org
  :commands (doct)
  :init (setq org-capture-templates
              (doct `(("Personal" :keys "p" :children
                       (("Todo"   :keys "t"
                         :template ("* TODO %^{Description}"
                                    "SCHEDULED: %U")
                         :headline "Tasks" :file ,(concat org-directory "/personal/tasks.org"))
                        ("Notes"  :keys "n"
                         :template ("* %^{Description}"
                                    ":PROPERTIES:"
                                    ":Created: %U"
                                    ":END:")
                         :headline "Notes" :file ,(concat org-directory "/personal/tasks.org"))
                        ("Appointment"  :keys "a"
                         :template ("* %^{Description}"
                                    "SCHEDULED: %T"
                                    ":PROPERTIES:"
                                    ":calendar-id: justincbarclay@gmail.com"
                                    ":END:")
                     :file ,(concat org-directory "/personal/calendar.org"))
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
                         :headline "Tasks" :file ,(concat org-directory "/work/tasks.org"))
                        ("PR Review"  :keys "p"
                         :template ("* TODO %^{Date}u" "%?")
                         :olp ("Review PRs")
                         :file ,(concat org-directory "/work/tasks.org"))
                        ("Notes"  :keys "n"
                         :template ("* %^{Description}"
                                    ":PROPERTIES:"
                                    ":Created: %U"
                                    ":END:")
                         :headline "Notes" :file ,(concat org-directory "/work/tasks.org"))
                        ("Emails" :keys "e"
                         :template "* TODO [#A] Reply: %a :@work:"
                         :headline "Emails" :file ,(concat org-directory "/work/tasks.org"))
                        ("Trello" :keys "r"
                         :template ("* TODO [#B] %a " "SCHEDULED: %U")
                         :headline "Tasks" :file ,(concat org-directory "/work/tasks.org"))
                        ("Appointment"  :keys "a"
                         :template ("* %^{Description}"
                                    "SCHEDULED: %T"
                                    ":PROPERTIES:"
                                    ":calendar-id: justin.barclay@tidalcloud.com"
                                    ":END:")
                         :file ,(concat org-directory "/work/calendar.org"))))))))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  '((?A :foreground "red")
    (?B :foreground "orange")
    (?C :foreground "blue"))
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/dev/diary")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n d" . org-roam-dailies-capture-today)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package org-noter
  :custom
  (org-noter-supported-mode '(doc-view-mode pdf-mode-view nov-mode))
  (org-noter-always-create-frame nil))

(use-package visual-fill-column)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . visual-line-mode)
         (nov-mode . visual-fill-column-mode)
         (nov-mode . (lambda () (display-line-numbers-mode -1)))
         (nov-mode . (lambda ()
                       (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                                                :height 1.0))))
  :bind (:map nov-mode-map
              ("h n" . nov-next-heading)
              ("h p" . nov-previous-heading)
              ("h j" . nov-jump-to-heading))
  :custom
  (nov-text-width t)
  (visual-fill-column-center-text t)
  :config
  (defun nov-next-heading ()
    (interactive)
    (text-property-search-forward 'outline-level))

  (defun nov-previous-heading ()
    (interactive)
    (text-property-search-forward 'outline-level))
  (defun nov-jump-to-heading ()
    (interactive)
    (let* ((headings '())
           selection)
      (save-mark-and-excursion
         (goto-char (point-min))
         (while-let ((prop (text-property-search-forward 'outline-level)))
           (push (list (buffer-substring-no-properties
                        (prop-match-beginning prop)
                        (prop-match-end prop))
                       (prop-match-beginning prop))
                 headings)))
      (setq selection (completing-read "Jump to heading: "
                                       headings))
      (when selection
       (goto-char (cadr (assoc selection headings)))))))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-download
  :after org
  :hook (org-mode . org-download-enable))

(use-package org-transclusion
  :after org)

(use-package org-transclusion-http
  :after org-transclusion)

(use-package org-auto-clock
  :vc (:url "https://github.com/justinbarclay/org-auto-clock" :rev :newest)
  :hook (after-init . org-auto-clock-mode)
  :custom
  (org-clock-idle-time 20)
  (org-auto-clock-projects '("tidal-wave" "application-inventory"))
  (org-auto-clock-project-name-function #'projectile-project-name))

(use-feature eshell
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

(use-package vterm
  :ensure nil
  :config
  (display-line-numbers-mode -1))

(use-package eat
  :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  :hook (eat-mode . (lambda () (setq display-line-numbers nil))))

(use-feature special-mode
  :hook (special-mode . (lambda () (display-line-numbers-mode -1))))

(use-package transient)

;; Magit is an Emacs interface to Git.
;; (It's awesome)
;; https://github.com/magit/magit
(use-package magit
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status))
  :hook
  (git-commit-mode . magit-commit-mode-init)
  :bind (:map magit-mode-map
              ("c" . magit-maybe-commit))
  :init
  (progn
    ;; magit extensions
    ;; stops the invalid style showing up.
    ;; From: http://git.io/rPBE0Q
    (defun magit-commit-mode-init ()
      "Force a new line to be inserted into a commit window"
      (when (looking-at "\n"))
      (open-line 1))
    (defun magit-maybe-commit (&optional show-options)
      "Runs magit-commit unless prefix is passed"
      (interactive "P")
      (if show-options
          (magit-key-mode-popup-committing)
        (magit-commit))))
  :config
  ;; make magit status go full-screen but remember previous window
  ;; settings
  ;; from: http://whattheemacsd.com/setup-magit.el-01.html
  (advice-add 'magit-status :around #'(lambda (orig-fun &rest args)
                                        (window-configuration-to-register :m)
                                        (apply orig-fun args)
                                        (delete-other-windows)))

  (advice-add 'git-commit-commit :after #'(lambda (&rest _)
                                            (delete-window)))

  (advice-add 'git-commit-abort :after #'(lambda (&rest _)
                                           (delete-window)))

  ;; restore previously hidden windows
  (advice-add 'magit-quit-window
               :around
               #'(lambda (oldfun)
                   (let ((current-mode major-mode))
                     (funcall oldfun)
                     (when (eq 'magit-status-mode current-mode)
                       (jump-to-register :m)))))
   ;; magit settings
  (setopt
   ;; customize the iconify function for
   magit-format-file-function #'magit-format-file-nerd-icons
   ;; don't put "origin-" in front of new branch names by default
   magit-default-tracking-name-function #'magit-default-tracking-name-branch-only
   ;; open magit status in same window as current buffer
   magit-status-buffer-switch-function #'switch-to-buffer
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

(use-package forge
  :after magit
  :init
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(use-package hl-todo)

(use-package magit-todos
  :custom
  (magit-todos-exclude-globs '("dist/**"))
  :hook (magit-mode . magit-todos-mode))

(use-package pos-tip)

(use-feature mu4e
  :commands (mu4e mu4e-update-mail-and-index)
  :bind (:map mu4e-headers-mode-map
              ("q" . kill-current-buffer))
  :after org
  :config
  (setq
   mu4e-headers-skip-duplicates  t
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-use-fancy-chars t
   mu4e-compose-format-flowed nil
   mu4e-date-format "%y/%m/%d"
   mu4e-headers-date-format "%Y/%m/%d"
   mu4e-change-filenames-when-moving t
   mu4e-attachments-dir "~/Downloads"
   mu4e-maildir       "~/Maildir/"   ;; top-level Maildir
   ;; note that these folders below must start with /
   ;; the paths are relative to maildir root

   ;; this setting allows to re-sync and re-index mail
   ;; by pressing U
   mu4e-get-mail-command "mbsync -a"

   mu4e-completing-read-function 'completing-read
   mu4e-context-policy 'pick-first
   mu4e-contexts (list
                  (make-mu4e-context
                   :name "fastmail"
                   :match-func
                   (lambda (msg)
                     (when msg
                       (string-prefix-p "/fastmail" (mu4e-message-field msg :maildir))))
                   :vars '((user-mail-address . "github@justinbarclay.ca")
                           (user-full-name    . "Justin Barclay")
                           (mu4e-drafts-folder  . "/fastmail/Drafts")
                           (mu4e-sent-folder  . "/fastmail/Sent")
                           (mu4e-refile-folder  . "/fastmail/Archive")
                           (sendmail-program . "msmtp")
                           (send-mail-function . smtpmail-send-it)
                           (message-sendmail-f-is-evil . t)
                           (message-sendmail-extra-arguments . ("--read-envelope-from"))
                           (message-send-mail-function . message-send-mail-with-sendmail)
                           (smtpmail-default-smtp-server . "smtp.fastmail.com")
                           (smtpmail-smtp-server  . "smtp.fastmail.com")
                           (mu4e-trash-folder  . "/fastmail/Trash")))))

  (display-line-numbers-mode -1))

;; (push 'mu4e elpaca-ignored-dependencies)

(use-package mu4e-dashboard
  :vc (:url "https://github.com/rougier/mu4e-dashboard" :rev :newest)
  :bind ("C-c d" . mu4e-dashboard)
  :after mu4e
  :hook
  (mu4e-dashboard-mode . (lambda ()
                           (display-line-numbers-mode -1)
                           (flyspell-mode -1)))
  :custom
  (mu4e-dashboard-file "~/.emacs.d/dashboards/mu4e-dashboard.org")
  :config
  (defun mu4e-dashboard-edit ()
    (interactive)
    (let ((edit-buffer "*edit-mu4e-dashboard*"))
      (when (get-buffer edit-buffer)
        (kill-buffer (get-buffer edit-buffer)))
      (make-indirect-buffer (current-buffer) edit-buffer)
      (switch-to-buffer-other-window (get-buffer edit-buffer))
      (org-mode 1))))

(use-package nano-mu4e
  :vc (:url "https://github.com/rougier/nano-mu4e" :rev :newest)
  :hook (mu4e-headers-mode . nano-mu4e-mode)
  :config
  (defun nano-mu4e-mode-on ()
   (setq mu4e-headers-append-func #'nano-mu4e-append-handler
         mu4e-found-func #'nano-mu4e-found-handler
         mu4e-headers-fields '((:nano-mu4e))
         mu4e--mark-fringe "")
   (advice-add #'mu4e-thread-fold-info
               :override #'nano-mu4e-thread-fold-info)
   (advice-add #'mu4e~headers-mark
               :override #'nano-mu4e-nop)
   (advice-add #'mu4e-mark-at-point
               :override #'nano-mu4e-mark-at-point)
   (advice-add #'mu4e-headers-mark-and-next
               :override #'nano-mu4e-headers-mark-and-next)
   (setq nano-mu4e-mode 1)))

(use-package elfeed
 :custom
 (elfeed-feeds
      '(("http://nullprogram.com/feed/" emacs)
        ("https://www.penny-arcade.com/feed" comics)
        ("https://sachachua.com/blog/feed/" emacs)
        ("https://macowners.club/posts/index.xml" emacs)
        ("https://fasterthanli.me/index.xml" tech rust)
        ("https://justinbarclay.ca/index.xml" mine)
        ("https://blog.1password.com/index.xml" security authentication)
        ("https://www.michaelgeist.ca/blog/feed/" canada law)
        ("https://popehat.substack.com/feed" law)
        ("https://www.joelonsoftware.com/feed/" tech)
        ("https://xeiaso.net/blog.rss" tech nix)
        ("https://byorgey.wordpress.com/feed/" functional-programming)
        ("https://mjg59.dreamwidth.org/" tech)
        ("https://oxide.computer/blog/feed" tech company))))

(use-package ligature
  :commands (global-ligature-mode)
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

(use-package lambda-themes
  :vc (:url "https://github.com/lambda-emacs/lambda-themes" :rev :newest)
  :defer t
  :custom
  (lambda-themes-set-italic-comments nil)
  (lambda-themes-set-italic-keywords nil)
  (lambda-themes-set-variable-pitch nil))

(use-package catppuccin-theme)

(use-package doom-themes
  :vc (:url "https://github.com/justinbarclay/themes" :rev "laserwave-hc")
  :init
  (add-to-list 'load-path (concat (expand-file-name package-user-dir)
                                  "/doom-themes/extensions"))
  (load-theme 'doom-laserwave-high-contrast t)
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Corrects (and improves) org-mode's native fontification.
  (require 'doom-themes-ext-org)
  (doom-themes-org-config))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after (nerd-icons marginalia)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package lambda-line
  :vc (:url "https://github.com/lambda-emacs/lambda-line" :rev :newest)
  :custom
  (lambda-line-position 'top) ;; Set position of status-line
  (lambda-line-abbrev t) ;; abbreviate major modes
  (lambda-line-hspace "  ")  ;; add some cushion
  (lambda-line-prefix t) ;; use a prefix symbol
  (lambda-line-prefix-padding nil) ;; no extra space for prefix
  (lambda-line-status-invert nil)  ;; no invert colors
  (lambda-line-git-diff-mode-line nil)
  (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
  (lambda-line-gui-mod-symbol " ⬤")
  (lambda-line-gui-rw-symbol  " ◯")
  (lambda-line-space-top +.25)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.25)
  (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  (lambda-line-syntax t)
  :hook (after-init . lambda-line-mode)
  :config
  (require 'nerd-icons)
  (setq lambda-line-flycheck-label (format " %s" (nerd-icons-mdicon "nf-md-alarm_light")))
  (setq lambda-line-vc-symbol (format " %s" (nerd-icons-mdicon "nf-md-git")))
  (setq lambda-line-mu4e t)
  ;; activate lambda-line
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))

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

(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package undo-fu)

(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode)
  :custom
  (undo-fu-session-file-limit 10))

(use-feature ibuffer
  :commands (ibuffer-current-buffer
             ibuffer-find-file
             ibuffer-do-sort-by-alphabetic)
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  (setq ibuffer-formats '((mark modified read-only locked
                                " " (icon 2 2 :left :elide) (name 18 18 :left :elide)
                                " " (size 9 -1 :right)
                                " " (mode 16 16 :left :elide) " " filename-and-process)
                          (mark " " (name 16 -1) " " filename))))

(use-package ibuffer-projectile
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  :config
  (setq ibuffer-projectile-prefix (concat
                                   (nerd-icons-octicon "nf-oct-file_directory"
                                                       :face ibuffer-filter-group-name-face
                                                       :v-adjust 0.1
                                                       :height 1.0)
                                   " ")))

(use-feature dired
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("a" . dired-find-file)))

(use-package dirvish
  :custom
  ;; Go back home? Just press `bh'
  (dirvish-bookmark-entries
   '(("h" "~/" "Home")
     ("m" "~/dev/tidal/application-inventory/" "MMP")
     ("t" "~/dev/tidal/tidal-wave" "Tidal Wave")))
  (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format ; it's ok to place string inside
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  ;; Don't worry, Dirvish is still performant even you enable all these attributes
  (dirvish-attributes '(nerd-icons file-size collapse subtree-state vc-state git-msg))
  ;; Maybe the icons are too big to your eyes
  (dirvish-nerd-icons-height 0.8)
  ;; In case you want the details at startup like `dired'
  ;; (dirvish-hide-details nil)
  :config
  (dirvish-peek-mode)
  (dirvish-override-dired-mode)
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

(use-package exec-path-from-shell
  :if jb/os-macos-p
  :defer 1
  :init
  (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments '("-l")))

(when jb/os-windows-p
  (setq package-check-signature nil)
  (require 'gnutls)
  (add-to-list 'gnutls-trustfiles (expand-file-name "~/.cert/cacert.pm"))
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))

(when (not jb/os-windows-p)
  (use-package envrc
    :defer 2
    :config
    (envrc-global-mode)))

(use-feature uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-feature recentf
  :init
  (recentf-mode)
  :custom ((recentf-save-file (concat user-emacs-directory ".recentf"))
           (recentf-max-menu-items 40)))

(use-package projectile
  :defer 1
  ;;:bind (("C-s p" . projectile-ripgrep))
  :commands
  (projectile-find-file projectile-switch-project projectile-ripgrep)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'auto)
  (setq projectile-enable-caching t)
  (add-to-list 'projectile-globally-ignored-directories "~")

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

  ;; (which-key-add-keymap-based-replacements projectile-command-map
  ;;   "4" (cons "other-window" projectile-other-window-map)
  ;;   "5" (cons "other-frame" projectile-other-frame-map)
  ;;   "s" (cons "search" projectile-search-map))
  )

(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-feature windmove-mode
  :ensure nil
  :commands (windmove-left windmove-right windmove-up windmove-down)
  :init
  (defvar-keymap windmove-custom-mode-map
    :repeat (:exit (ignore))
    "<down>" #'windmove-down
    "<up>" #'windmove-up
    "<left>" #'windmove-left
    "<right>" #'windmove-right)
  (set-keymap-parent windmove-custom-mode-map window-prefix-map)
  (keymap-global-set "C-x w" windmove-custom-mode-map))

(use-feature repeat-mode
  :init (repeat-mode))

(unbind-key "C-s")

(use-package rg
  :bind (("C-s r" . rg)))

(use-package avy
  :bind (("C-s a" . #'avy-goto-char-timer))
  :custom
  (avy-enter-times-out 't)
  (avy-timeout-seconds 1))

(use-feature isearch
  :bind (("C-s i" . isearch-forward-regexp)
         :map isearch-mode-map
              ("M-j" . avy-isearch)))

(use-feature occur
  :bind ("C-s o" . occur))

(use-feature multi-occur
  :init
  (defun get-buffers-matching-mode (mode)
    "Returns a list of buffers where their major-mode is equal to MODE"
    (let ((buffer-mode-matches '()))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (eq mode major-mode)
            (push buf buffer-mode-matches))))
      buffer-mode-matches))
  (defun multi-occur-in-this-mode ()
    (interactive)
    (multi-occur (get-buffers-matching-mode major-mode)
                 (car (occur-read-primary-args))))
  :bind ("C-s m" . multi-occur-in-this-mode))

(use-feature emacs
  :config

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq-default truncate-lines t)

(global-hl-line-mode 1)

(show-paren-mode 1)

(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)

(setq-default c-basic-offset 2)

(save-place-mode 1)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)
(setq backup-by-copying t)

(setq-default warning-suppress-log-types '((copilot copilot-no-mode-indent)))

(unbind-key "M-,")

(defun pop-mark-dwim ()
  "If xref history exist, use that to move around and if not pop off the global mark stack."
  (interactive)
  (condition-case nil
      (xref-go-back)
    (user-error
     (pop-global-mark))))

(bind-key "M-," #'pop-mark-dwim)

)

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :bind (:map smartparens-mode-map ("M-<backspace>" . 'backward-kill-word)))

(use-feature smartparens-config
  :after smartparens)

(use-package stripspace
  :ensure t

  ;; Enable for prog-mode-hook, text-mode-hook, conf-mode-hook
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))

  :custom
  ;; The `stripspace-only-if-initially-clean' option:
  ;; - nil to always delete trailing whitespace.
  ;; - Non-nil to only delete whitespace when the buffer is clean initially.
  ;; (The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.)
  (stripspace-only-if-initially-clean nil)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (stripspace-restore-column t))

(use-package origami
  :defer t
  :bind ("C-<tab>" . origami-recursively-toggle-node)
  :hook (prog-mode . origami-mode))

(use-package hungry-delete
  :hook (prog-mode . global-hungry-delete-mode))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this))
  :commands (mc/mark-next-like-this mc/mark-previous-like-this))

(use-package flycheck-posframe
  :hook ((flycheck-mode . flycheck-posframe-mode)
         (lsp-mode . (lambda () (flycheck-posframe-mode 0)))
         (post-command . flycheck-posframe-monitor-post-command))
  :custom
  (flycheck-posframe-warning-prefix "⚠ ")
  (flycheck-posframe-error-prefix "❌ ")
  (flycheck-posframe-info-prefix "ⓘ ")
  :config
  (defun flycheck-posframe-monitor-post-command ()
    (when (not (flycheck-posframe-check-position))
      (posframe-hide flycheck-posframe-buffer)))
  (set-face-attribute 'flycheck-posframe-info-face nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error))

(use-package flycheck-package
  :init
  (use-package package-lint))

(use-package flycheck
  :init
  (defun flycheck-node-modules-executable-find (executable)
    (or
     (let* ((base (locate-dominating-file buffer-file-name "node_modules"))
            (cmd  (if base (expand-file-name (concat "node_modules/.bin/" executable)  base))))
       (if (and cmd (file-exists-p cmd))
           cmd))
     (flycheck-default-executable-find executable)))

  (defun flycheck-node-modules-hook ()
    "Look inside node modules for the specified checker"
    (setq-local flycheck-executable-find #'flycheck-node-modules-executable-find))

  (global-flycheck-mode)
  :hook
  ((typescript-ts-base-mode
    js-base-mode
    web-mode
    css-ts-mode
    less-css-mode) .  #'flycheck-node-modules-hook)
  :custom
  (checkdoc-force-docstrings-flag nil)
  ;; (flycheck-javascript-eslint-executable "eslint_d")
  ;; (flycheck-typescript-tslint-executable "eslint_d")
  (flycheck-check-syntax-automatically '(save idle-change idle-buffer-switch mode-enabled))
  (flycheck-standard-error-navigation nil)
  (flycheck-stylelintrc ".stylelintrc.json"))

(use-feature flyspell
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :config (setq flyspell-issue-message-flag nil))

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

(use-feature savehist
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
  (("C-s b" . consult-line)
   ;; C-x bindings (ctl-x-map)
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
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
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

(use-package consult-omni
  :vc (:url "https://github.com/justinbarclay/consult-omni" :branch "main" :rev :newest)
  :commands (consult-omni-multi consult-omni-apps)
  :init
  (add-to-list 'load-path (concat (expand-file-name package-user-dir)
                              "/consult-omni/sources/"))
  :custom
  ;; General settings that apply to all sources
  (consult-omni-show-preview t) ;;; show previews
  (consult-omni-preview-key "C-o") ;;; set the preview key to C-o
  (consult-omni-group-by :source)
  :config
  ;; Load Sources Core code
  (require 'consult-omni-sources)
  ;; Load Embark Actions
  (require 'consult-omni-embark)
  (when jb/os-macos-p
    (add-to-list 'consult-omni-apps-paths "/Applications/Nix Apps"))

  ;; Either load all source modules or a selected list

   ;;; Select a list of modules you want to aload, otherwise all sources all laoded
  (setq consult-omni-sources-modules-to-load
        (list 'consult-omni-apps
              'consult-omni-buffer
               'consult-omni-calc
               'consult-omni-dict
               'consult-omni-fd
               'consult-omni-google
               'consult-omni-google-autosuggest
               'consult-omni-gptel
               'consult-omni-line-multi
               ;;'consult-omni-org-agenda
               'consult-omni-ripgrep
               'consult-omni-ripgrep-all
               'consult-omni-wikipedia
               'consult-omni-youtube))

  (consult-omni-sources-load-modules)
   ;;; set multiple sources for consult-omni-multi command. Change these lists as needed for different interactive commands. Keep in mind that each source has to be a key in `consult-omni-sources-alist'.
  (setq consult-omni-multi-sources '("calc"
                                     "File"
                                     "Buffer"
                                     ;; "Bookmark"
                                     "Apps"
                                     "gptel"
                                     "Dictionary"
                                     "Google"
                                     "Wikipedia"
                                     ;; "elfeed"
                                     "mu4e"
                                     "buffers text search"
                                     "Org Agenda"
                                     ;; "GitHub"
                                     "YouTube"))
                                     ;; "Invidious"))

  ;;; Pick you favorite autosuggest command.
  (setq consult-omni-default-autosuggest-command #'consult-omni-dynamic-google-autosuggest) ;;or any other autosuggest source you define

  ;;; Set your shorthand favorite interactive command
  (setq consult-omni-default-interactive-command #'consult-omni-multi))

(use-package company
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line)
  :hook (after-init . global-company-mode)
  :bind (("C-<shift>-<tab>" . company-manual-begin)
         :map company-active-map
         ("C->" . #'company-filter-candidates)
         ("C-/" . #'company-other-backend))
  :config
  (require 'company-files)
  (setq company-minimum-prefix-length 1
        company-tooltip-limit 14
        company-idle-delay 0.1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not erc-mode
                                   circe-mode
                                   message-mode
                                   help-mode
                                   gud-mode
                                   vterm-mode)
        company-frontends '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
                            company-echo-metadata-frontend)  ; show selected candidate docs in echo area

        ;; Buffer-local backends will be computed when loading a major mode, so
        ;; only specify a global default here.
        company-backends '((company-capf :separate company-dabbrev)
                           company-files
                           company-yasnippet)

        ;; These auto-complete the current selection when
        ;; `company-auto-commit-chars' is typed. This is too magical. We
        ;; already have the much more explicit RET and TAB.
        company-auto-commit nil

        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)"))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :init
  (require 'nerd-icons)
  (setq company-box-icons-alist 'company-box-icons-nerd-icons
        company-box-icons-nerd-icons
      `((Unknown        . ,(nerd-icons-codicon  "nf-cod-text_size"           :face  'font-lock-warning-face))
        (Text           . ,(nerd-icons-codicon  "nf-cod-text_size"           :face  'font-lock-doc-face))
        (Method         . ,(nerd-icons-codicon  "nf-cod-symbol_method"       :face  'font-lock-function-name-face))
        (Function       . ,(nerd-icons-codicon  "nf-cod-symbol_method"       :face  'font-lock-function-name-face))
        (Constructor    . ,(nerd-icons-codicon  "nf-cod-triangle_right"      :face  'font-lock-function-name-face))
        (Field          . ,(nerd-icons-codicon  "nf-cod-symbol_field"        :face  'font-lock-type-face))
        (Variable       . ,(nerd-icons-codicon  "nf-cod-symbol_variable"     :face  'font-lock-type-face))
        (Class          . ,(nerd-icons-codicon  "nf-cod-symbol_class"        :face  'font-lock-type-face))
        (Interface      . ,(nerd-icons-codicon  "nf-cod-symbol_interface"    :face  'font-lock-type-face))
        (Module         . ,(nerd-icons-codicon  "nf-cod-file_submodule"      :face  'font-lock-preprocessor-face))
        (Property       . ,(nerd-icons-codicon  "nf-cod-symbol_property"     :face  'font-lock-variable-name-face))
        (Unit           . ,(nerd-icons-codicon  "nf-cod-symbol_ruler"        :face  'font-lock-constant-face))
        (Value          . ,(nerd-icons-codicon  "nf-cod-symbol_field"        :face  'font-lock-builtin-face))
        (Enum           . ,(nerd-icons-codicon  "nf-cod-symbol_enum"         :face  'font-lock-builtin-face))
        (Keyword        . ,(nerd-icons-codicon  "nf-cod-symbol_keyword"      :face  'font-lock-keyword-face))
        (Snippet        . ,(nerd-icons-codicon  "nf-cod-notebook_template"      :face  'font-lock-string-face))
        (Color          . ,(nerd-icons-codicon  "nf-cod-symbol_color"        :face  'success))
        (File           . ,(nerd-icons-codicon  "nf-cod-symbol_file"         :face  'font-lock-string-face))
        (Reference      . ,(nerd-icons-codicon  "nf-cod-references"          :face  'font-lock-variable-name-face))
        (Folder         . ,(nerd-icons-codicon  "nf-cod-folder"              :face  'font-lock-variable-name-face))
        (EnumMember     . ,(nerd-icons-codicon  "nf-cod-symbol_enum_member"  :face  'font-lock-builtin-face))
        (Constant       . ,(nerd-icons-codicon  "nf-cod-symbol_constant"     :face  'font-lock-constant-face))
        (Struct         . ,(nerd-icons-codicon  "nf-cod-symbol_structure"    :face  'font-lock-variable-name-face))
        (Event          . ,(nerd-icons-codicon  "nf-cod-symbol_event"        :face  'font-lock-warning-face))
        (Operator       . ,(nerd-icons-codicon  "nf-cod-symbol_operator"     :face  'font-lock-comment-delimiter-face))
        (TypeParameter  . ,(nerd-icons-codicon  "nf-cod-list_unordered"      :face  'font-lock-type-face))
        (Template       . ,(nerd-icons-codicon  "nf-cod-notebook_template"      :face  'font-lock-escape-face))
        (ElispFunction  . ,(nerd-icons-codicon  "nf-cod-symbol_method"       :face  'font-lock-function-name-face))
        (ElispVariable  . ,(nerd-icons-codicon  "nf-cod-symbol_variable"     :face  'font-lock-type-face))
        (ElispFeature   . ,(nerd-icons-codicon  "nf-cod-globe"               :face  'font-lock-builtin-face))
        (ElispFace      . ,(nerd-icons-codicon  "nf-cod-symbol_color"        :face  'success))))

  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-tooltip-limit 50
        ;; Move company-box-icons--elisp to the end, because it has a catch-all
        ;; clause that ruins icons from other backends in elisp buffers.
        company-box-icons-functions
        (cons #'+company-box-icons--elisp-fn
              (delq 'company-box-icons--elisp
                    company-box-icons-functions)))


  (setq company-box-scrollbar nil)

  (defun +company-box-icons--elisp-fn (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
              ((boundp sym)   'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep sym)    'ElispFace))))))

(use-package yasnippet
 :init (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package apheleia
  :init
  (apheleia-global-mode)
  :config
  ;; Override ruby-ts-mode defaults
  (map-put! apheleia-formatters 'rustfmt
      '("rustfmt" "--edition" (or (bound-and-true-p rust-edition) "2024")
        "--quiet" "--emit" "stdout"))
  (map-put! apheleia-mode-alist 'ruby-ts-mode '(rubocop)))

(use-feature treesit
  :config
  (setq-default treesit-font-lock-level 4))

(use-package treesit-auto
  :if (and (require 'treesit)
           (treesit-available-p))
  :defer 1
  :commands (make-treesit-auto-recipe)
  :custom
  (treesit-auto-install 'prompt)
  :init
  (setq my-jsdoc-tsauto-config
    (make-treesit-auto-recipe
     :lang 'jsdoc
     :ts-mode 'js-ts-mode
     :url "https://github.com/tree-sitter/tree-sitter-jsdoc"
     :revision "master"
     :source-dir "src"
     :requires 'javascript))
  (setq my-js-tsauto-config
   (make-treesit-auto-recipe
    :lang 'javascript
    :ts-mode 'js-ts-mode
    :remap '(js2-mode js-mode javascript-mode)
    :url "https://github.com/tree-sitter/tree-sitter-javascript"
    :revision "master"
    :requires 'jsdoc
    :source-dir "src"
    :ext "\\.js\\'"))
  (add-to-list 'treesit-auto-recipe-list my-js-tsauto-config)
  (add-to-list 'treesit-auto-recipe-list my-jsdoc-tsauto-config)
  (add-to-list 'treesit-auto-langs 'jsdoc)
  :config
  (global-treesit-auto-mode))

(use-package copilot
  :after jsonrpc
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)))

(use-package gptel
  :after 1password
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model 'gemini-2.5-pro-preview-05-06)

  :config
  (setq gptel-backend (gptel-make-gemini "gemini"
                        :key (string-trim (aio-wait-for (1password--read "Gemini" "credential" "private")))
                        :stream t))
  (defvar meeting-minutes-prompt "Purpose and Goals:
* Act as a professional meeting secretary.
* Generate accurate and comprehensive meeting minutes based on provided information.
* Structure the minutes logically and professionally.

Behaviours and Rules:

1) Minute Generation:
   a) Organize the minutes with clear headings: 'Meeting Title', 'Date & Time', 'Location/Platform', 'Attendees', 'Absent', 'Agenda Items Discussed', 'Key Decisions Made', 'Action Items', and 'Next Meeting'.
   b) For 'Agenda Items Discussed', provide a concise summary of the discussion for each item.
   c) For 'Key Decisions Made', list all decisions clearly and concisely.
   d) For 'Action Items', use a table format: | Task | Owner | Deadline | to ensure clarity and accountability.
   e) If a 'Next Meeting' is mentioned, include the date, time, and location/platform (if specified).
   f) If a user asks you to export the meeting to org-mode put the following headings as properties on the Meeting Title: 'Date & Time', 'Location/Platform', 'Attendees', 'Absent'

2) Tone and Style:
   a) Maintain a professional and objective tone throughout the minutes.
   b) Use clear, concise, and grammatically correct language.
   c) Avoid personal opinions, interpretations, or unnecessary details.
   d) Present information factually and neutrally.
   e) If there is banter about non-work related things, keep it off in its own section

3) Information Handling:
   a) Accurately record all essential information provided.
   b) Ask for clarification if any information is unclear or missing.
   c) Ensure all sections of the minutes are populated based on the information available.

Overall Tone:
* Professional
* Objective
* Organized
* Detail-oriented")
  (add-to-list 'gptel-directives (list 'meeting-minutes meeting-minutes-prompt))

  (defun gptel-summarize-meeting-minutes--callback (response info)
    (if (stringp response)
        (let ((posn (marker-position (plist-get info :position)))
              (buf  (buffer-name (plist-get info :buffer)))
              sanitized-response)
          (with-temp-buffer
            (insert response)
            (goto-char (point-min))
            (activate-mark)
            (while (re-search-forward "^```org" nil t)
              (delete-region (point-min) (match-end 0)))
            (while (re-search-forward "^```" nil t)
              (replace-match ""))
            (setq sanitized-response (buffer-substring-no-properties (point-min)
                                                                     (point-max))))
          (with-current-buffer buf
            (save-excursion
              (goto-char posn)
              (insert sanitized-response)))
          (message "gptel-request failed with message: %s"
                   (plist-get info :status)))))

  (defun gptel-summarize-meeting-minutes ()
    ;; get file name from a bookmark or other
    (interactive)
    (org-narrow-to-subtree)
    (let* ((bookmark-p (yes-or-no-p "Use a bookmark?"))
           (bookmark (when bookmark-p
                       (bookmark-completing-read "Bookmark: ")))
           (dir (when bookmark
                  (directory-file-name (bookmark-get-filename bookmark))))
           (file-name (read-file-name "Meeting file: " dir))
           (query-text "Can you summarize this meeting? After you are done summarizing it export it as an org-mode document."))
      (gptel-add-file file-name)
      (let* ((gptel-use-curl)
             (gptel-use-context 'system)
             (gptel-model 'gemini-2.5-pro-exp-03-25))
        (gptel-request query-text
         :system (cadr
                  (assq 'meeting-minutes gptel-directives))
         :context 'system
         :callback #'gptel-summarize-meeting-minutes--callback))
      (gptel-remove-file file-name))))

(use-package aidermacs
  :bind (("C-c C-a" . aidermacs-transient-menu))
  :config
  (setenv "GEMINI_API_KEY" (string-trim (aio-wait-for (1password--read "Gemini" "credential" "private"))))
  (setenv "ANTHROPIC_API_KEY" (string-trim (aio-wait-for (1password--read "Claude" "credential" "private"))))
  (aidermacs-setup-minor-mode)
  :custom
  (aidermacs-auto-commits t)
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "gemini/gemini-2.5-pro-exp-03-25"))

(use-package lsp-mode
  :commands lsp
  :hook ((rustic-mode
          rust-base-mode
          web-mode
          ruby-base-mode
          c-mode
          js-base-mode
          json-ts-mode
          typescript-ts-mode
          lua-mode
          jsx-ts-mode
          tsx-ts-mode
          less-css-mode
          css-ts-mode
          go-base-mode
          LaTeX-mode
          nix-mode
          org-mode)
         . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-idle-delay 0.1)
  (lsp-log-io nil)
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-solargraph-use-bundler 't)
  (lsp-keymap-prefix "C-l")
  (lsp-diagnostic-clean-after-change 't)
  (lsp-copilot-enabled 't)
  :config
  (defvar lsp-flycheck-mapping '(less-css-mode (less-stylelint less)
                                 css-base-mode (css-stylelint)
                                 js-base-mode (javascript-eslint)
                                 typescript-ts-base-mode (javascript-eslint)
                                 tsx-ts-mode (javascript-eslint)
                                 jsx-ts-mode (javascript-eslint))
                              "a selections of major modes and the associated checkers to run after lsp
runs it's diagnostics.")
  (defvar-local my/flycheck-local-cache nil)

  (defun my/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker my/flycheck-local-cache))
        (funcall fn checker property)))

  (advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (when-let ((checkers (plist-get lsp-flycheck-mapping major-mode)))
                (setq my/flycheck-local-cache `((lsp . ((next-checkers . ,checkers))))))))
  (setopt flycheck-error-list-minimum-level nil)
  (setopt flycheck-relevant-error-other-file-show 't)
  (setopt flycheck-relevant-error-other-file-minimum-level 'warning)
  ;; As stolen from https://github.com/emacs-lsp/lsp-mode/issues/3279
  (defun lsp-diagnostics--flycheck-start (checker callback)
   "Start an LSP syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."

   (remove-hook 'lsp-on-idle-hook #'lsp-diagnostics--flycheck-buffer t)

   (->> (lsp--get-buffer-diagnostics)
        (-mapcat
         (-lambda ((&Diagnostic :message :severity? :tags? :code? :source? :related-information?
                                :range (&Range :start (&Position :line      start-line
                                                                 :character start-character)
                                               :end   (&Position :line      end-line
                                                                 :character end-character))))
           (let ((group (gensym)))
             (cons (flycheck-error-new
                    :buffer (current-buffer)
                    :checker checker
                    :filename buffer-file-name
                    :message message
                    :level (lsp-diagnostics--flycheck-calculate-level severity? tags?)
                    :id code?
                    :group group
                    :line (lsp-translate-line (1+ start-line))
                    :column (1+ (lsp-translate-column start-character))
                    :end-line (lsp-translate-line (1+ end-line))
                    :end-column (1+ (lsp-translate-column end-character)))
                   (-mapcat
                    (-lambda ((&DiagnosticRelatedInformation
                               :message
                               :location
                               (&Location :range (&Range :start (&Position :line      start-line
                                                                           :character start-character)
                                                         :end   (&Position :line      end-line
                                                                           :character end-character))
                                          :uri)))
                      `(,(flycheck-error-new
                          :buffer (current-buffer)
                          :checker checker
                          :filename (-> uri lsp--uri-to-path lsp--fix-path-casing)
                          :message message
                          :level (lsp-diagnostics--flycheck-calculate-level (1+ severity?) tags?)
                          :id code?
                          :group group
                          :line (lsp-translate-line (1+ start-line))
                          :column (1+ (lsp-translate-column start-character))
                          :end-line (lsp-translate-line (1+ end-line))
                          :end-column (1+ (lsp-translate-column end-character)))))
                    related-information?)))))
        (funcall callback 'finished)))

 (defgroup lsp-harper nil
   "Settings for harper grammar language Server."
   :prefix "lsp-harper-"
   :group 'lsp-mode)

 (defcustom lsp-harper-active-modes
   '( rust-mode python-mode ess-mode typst-ts-mode markdown-mode)
   "List of major modes that work with lsp-ai"
   :type 'list
   :group 'lsp-harper)


 (defcustom lsp-harper-configuration
         '(:userDictPath ""
           :fileDictPath ""
           :linters (:SpellCheck t
                     :SpelledNumbers :json-false
                     :AnA t
                     :SentenceCapitalization t
                     :UnclosedQuotes t
                     :WrongQuotes :json-false
                     :LongSentences t
                     :RepeatedWords t
                     :Spaces t
                     :Matcher t
                     :CorrectNumberSuffix t)
           :codeActions (:ForceStable :json-false)
           :markdown (:IgnoreLinkTitle :json-false)
           :diagnosticSeverity "hint"
           :isolateEnglish :json-false)
         "Harper configuration structure"
         :type 'dictionary
         :group 'lsp-harper)


 (lsp-register-client
   (make-lsp-client
     :new-connection (lsp-stdio-connection
                      '("harper-ls" "-s"))
     :major-modes lsp-harper-active-modes
     :initialization-options lsp-harper-configuration
     :add-on? 't
     :priority -3
     :server-id 'lsp-harper)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package devdocs)

(use-feature css-mode
  :config
  (setq css-indent-offset 2))

(use-feature less-css-mode
  :config
  (setq css-indent-offset 2))

(use-package rainbow-mode
  :hook ((css-mode . rainbow-mode)
         (less-mode . rainbow-mode)))

(use-package sass-mode
  :mode "\\.sass\\'")

(use-feature c-ts-base-mode
  :config
  (progn ; C mode hook
    (add-hook 'c-mode-hook 'flycheck-mode)))

(use-feature c++-ts-mode)

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
  :commands (parinfer-rust-mode)
  :hook (emacs-lisp-mode)
  :custom
  (parinfer-rust-disable-troublesome-modes 't)
  (parinfer-rust-check-before-enable 'defer)
  (parinfer-rust-auto-download nil)
  :config
  (add-hook 'vundo-pre-enter-hook (lambda () (parinfer-rust-mode 0) nil t))
  (add-hook 'vundo-post-exit-hook (lambda () (parinfer-rust-mode 1) nil t))
  (add-hook 'parinfer-rust-mode-hook 'parinfer-rust--auto-apply-fast-mode))

(use-feature eldoc
  :config
  (global-eldoc-mode))

(use-feature elisp-mode
  :hook (emacs-lisp . enable-paredit))

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

(use-package janet-mode)

(use-feature js-base-mode
 :custom
 (js-indent-level 2)
 (lsp-eslint-enable 't))

(use-feature js-ts-mode
  :mode ("\\.js\\'" "\\.cjs\\'" "\\.mjs\\'"))

(use-feature typescript-ts-base-mode
  :custom
  (typescript-indent-level 2)
  (lsp-eslint-enable 't)
  :config
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-base-mode))

(use-feature typescript-ts-mode
  :mode ("\\.ts\\'" "\\.mts\\'" "\\.ts.snap\\'"))

(use-feature tsx-ts-mode
  :mode "\\.tsx\\'")

(use-package deno-fmt)

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
        web-mode-enable-current-element-highlight t)
        (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package tagedit
 :defer t)

(use-feature sgml-mode
  :after tagedit
  :config
  (require 'tagedit)
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'html-mode-hook (lambda () (tagedit-mode 1))))

(use-feature ruby-ts-mode
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby"
  :bind (:map ruby-ts-mode-map
              ("C-c r b" . 'treesit-beginning-of-defun)
              ("C-c r e" . 'treesit-end-of-defun))
  :hook (ruby-base-mode . subword-mode)
  :custom (ruby-indent-level 2)
          (ruby-indent-tabs-mode nil))

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

(use-package rspec-mode
 :hook (ruby-base-mode . rspec-enable-appropriate-mode)
 :config
 (rspec-install-snippets))

(use-package rustic
  :mode (("\\.rs\\'" . rustic-mode))
  :hook (rustic-mode . find-rust-version)
  :custom
  (rustic-lsp-setup-p '())
  (rustic-indent-offset 2)
  :config
  (defun find-rust-version ()
    "Get the Rust version specified in the Cargo.toml file."
    (when-let*
        ((cratedir
          (locate-dominating-file default-directory
                                  "Cargo.toml"))
         (manifest-path
          (expand-file-name "Cargo.toml" cratedir))
         (env process-environment)
         (path exec-path)
         (_ (executable-find rustic-cargo-bin)))
      (setq-local rust-edition
                  (with-temp-buffer
                    (setq-local process-environment env)
                    (setq-local exec-path path)
                    (let ((retcode
                           (process-file
                            rustic-cargo-bin nil (list (current-buffer) nil) nil
                            "metadata" "--no-deps" "--frozen" "--format-version" "1")))
                      (when (/= retcode 0)
                        (error "Cargo metadata --format-version 1 returned %s: %s"
                               retcode (buffer-string)))
                      (goto-char 0)
                      (let ((metadata (json-parse-buffer
                                       :object-type 'alist
                                       :array-type 'list)))
                        (cdr (assq 'edition
                                   (--first (string= (cdr (assq 'manifest_path it)) manifest-path)
                                            (cdr (assq 'packages metadata)))))))))))
  :config
  (electric-pair-mode 1))

(use-package nix-mode)

(use-package nixos-options)

(use-package nixpkgs-fmt
 :hook (nix-mode . nixpkgs-fmt-on-save-mode))

(use-feature go-ts-mode
  :mode "\\.go\\'"
  :custom
  (go-ts-mode-indent-offset 2)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package lua-mode)

(use-feature json-ts-mode
  :mode "\\.json\\'"
  :config
  (setq js-indent-level 2))

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :config
  (setq-local tab-width 2))

(use-package docker)

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

(use-package nushell-ts-mode
  :mode "\\.nu\\'")

(use-package terraform-mode
:mode "\\.tf\\'" )

(use-package yaml-mode
  :defer t)

(use-package ssh-config-mode
  :defer t)

;; (use-package sqlint)

(use-feature auctex
  :mode (("\\.tex\\'" . TeX-latex-mode)
         ("\\.tex\\.erb\\'" . TeX-latex-mode)
         ("\\.etx\\'" . TeX-latex-mode))
  :hook
  ((LaTeX-mode . turn-on-auto-fill)
   (LaTeX-mode . TeX-source-correlate-mode))
  :config
  (setq-default TeX-master nil)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :custom
  (LaTeX-item-indent 0)
  (TeX-auto-local ".auctex-auto")
  (TeX-style-local ".auctex-style")
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  ;; use pdflatex
  (TeX-PDF-mode t)
  (TeX-source-correlate-start-server nil)
  (TeX-source-correlate-method 'synctex)
  ;; use evince for dvi and pdf viewer
  ;; evince-dvi backend should be installed
  (TeX-view-program-selection
   '((output-dvi "DVI Viewer")
     (output-pdf "PDF Tools")
     (output-html "Firefox")))
  (TeX-view-program-list
   '(("DVI Viewer" "evince %o")
     ("PDF Tools" TeX-pdf-tools-sync-view))))

(use-package cdlatex)

(use-package pdf-tools
 :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
 :custom
 (pdf-tools-installer-os "nixos"))

(use-feature text-mode
  :custom
  (text-mode-ispell-word-completion nil))

(use-feature rst
  :mode (("\\.txt$" . rst-mode)
         ("\\.rst$" . rst-mode)
         ("\\.rest$" . rst-mode)))

(use-package kotlin-ts-mode)

(use-package android-env)

(use-feature tramp
  :defer t
  :hook (tramp-mode . (lambda () (projectile-mode 0)))
  :config (setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
  :custom
  (tramp-terminal-type "tramp")
  (tramp-use-ssh-controlmaster-options nil))

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

(use-package yequake
  :config
  (add-to-list 'yequake-frames '("consult-omni-demo"
                                 (buffer-fns . #'consult-omni-apps)
                                 (width . 0.8)
                                 (height . 0.1)
                                 (top . 0.5)
                                 (frame-parameters . ((name . "yequake-demo")
                                                      (minibuffer . only)
                                                      (autoraise . t)
                                                      (window-system . ns)))))) ;;change accordingly

(use-package graphviz-dot-mode)

(use-package 1password
  :vc (:url "https://github.com/justinbarclay/1password.el.git" :rev :newest)
  :commands (1password-search-password 1password-search-id 1password-enable-auth-source)
  :hook (after-init . 1password-enable-auth-source)
  :custom
  (1password-results-formatter '1password-colour-formatter)
  (1password-executable (if (executable-find "op.exe")
                            "op.exe"
                          "op")))

(use-package helpful
  :defer t
  :bind  (("C-h f" . helpful-callable)
          ("C-h v" . helpful-variable)
          ("C-h k" . helpful-key)))

(use-feature which-key
  :defer 't
  :init (which-key-mode))

(use-feature woman
  :config
  (progn (setq woman-manpath
              (split-string (shell-command-to-string "man --path") ":" t "\n"))
        (autoload 'woman "woman"
          "Decode and browse a UN*X man page." t)
        (autoload 'woman-find-file "woman"
          "Find, decode and browse a specific UN*X man-page file." t)))

(add-hook 'after-init-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time)))
                     gcs-done)))

(use-package esup
  :commands (esup))

(use-feature profiler
  :bind
  (("s-l" . profiler-start)
   ("s-r" . profiler-report)))

(use-package restclient)

(use-package git-link)

(use-package git-sync-mode
  :commands (git-sync-mode git-sync-global-mode)
  :vc (:url "https://github.com/justinbarclay/git-sync-mode" :rev :newest))

(use-package ctables
 :vc (:url "https://github.com/kiwanami/emacs-ctable" :rev :newest))

(use-package leetcode
 :commands (leetcode-show)
 :vc (:url "https://github.com/ginqi7/leetcode-emacs" :rev :newest)
 :config
 (setq leetcode-language "rust"))

(defmacro comment (docstring &rest body)
  "Ignores body and yields nil"
  nil)

(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.d/README.org"))

(global-set-key (kbd "C-c i") 'open-config-file)

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

(defun jb/copy-image-as-png ()
  (interactive)
  (let* ((file-name (string-trim (shell-command-to-string "wslpath -w ./test.png")))
         (command (format "%s; %s %s %s %s"
                         "Add-Type -AssemblyName System.Windows.Forms"
                         "[System.Windows.Forms.Clipboard]::GetDataObject().ContainsImage()"
                         "-and"
                         (format "[System.Windows.Forms.Clipboard]::GetImage().Save(\"%s\", \"Png\")"
                                 file-name)
                         "-and 1")))
    (shell-command (format "powershell.exe -c '%s'" command))))

(setq file-name-handler-alist doom--file-name-handler-alist)

(use-feature async-upgrade
  :after package
  :init
  (require 'subr-x)
  (require 'cl-lib)
  (require 'package)
  (defun async-upgrade--filter-system-packages (pkg)
    (string-prefix-p (file-name-as-directory
                      (expand-file-name package-user-dir))
                     (expand-file-name
                      (thread-first pkg
                                    (assoc package-alist)
                                    cadr
                                    (package-desc-dir)
                                    (expand-file-name)))))

  (defun async-upgrade--dependency-graph (package &optional tree)
    (let* ((dependency-tree (or tree
                                (make-hash-table)))
           (dependencies (mapcar
                          #'car
                          (package-desc-reqs
                           (cadr (assoc package package-alist)))))
           (current-package (gethash package dependency-tree (make-hash-table))))
      (puthash :dependencies dependencies current-package)
      (puthash package current-package dependency-tree)
      (mapc (lambda (dependent)
              (let* ((description (gethash dependent dependency-tree (make-hash-table)))
                     (dependents (gethash :dependents description '())))
                (puthash :dependents
                         (cons package dependents)
                         description)
                (puthash dependent
                         description
                         dependency-tree)))
            dependencies)
      dependency-tree))

  (defun async-upgrade--reload-packages (packages)
    (let ((activated-packages (seq-filter #'featurep
                                           packages)))
       (message "Reloading packages %s" activated-packages)
       (mapc (lambda (package) (unload-feature package 't))
             activated-packages)
      (mapc #'require activated-packages)))

  (defun async-upgrade-packages ()
    "Run package upgrades in a background Emacs process."
    (interactive)
    (let ((upgradeable (seq-filter
                        #'async-upgrade--filter-system-packages
                        (package--upgradeable-packages))))
      (if-let* ((buffer (get-buffer-create "*package-upgrade*"))
                ((yes-or-no-p (format "Upgrade %s" upgradeable))))
          (with-current-buffer buffer
            (defvar-local packages upgradeable)
            (erase-buffer)
            (special-mode)))
      (make-process
       :name "emacs-package-upgrade"
       :buffer "*package-upgrade*"
       :command (list (expand-file-name invocation-name invocation-directory)
                      "--script" (expand-file-name "upgrade-script.el" user-emacs-directory)
                      "--" (format "%s" upgradeable))
       :sentinel (lambda (process event)
                   (when (string= event "finished\n")
                     (async-upgrade--reload-packages packages)
                     (message "Package upgrade complete!")))))))

(defun count-repititions ()
  (interactive)
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

(defun kebab-case (string)
  "Convert STRING to kebab-case. For example, HelloWorld! becomes hello-world! Note that this downcases the first character but does not add a - before it"
  (let ((case-fold-search nil))
    (-> (replace-regexp-in-string
         "\\([A-Z]\\)"
         "-\\1"
         string)
      (downcase)
      (string-trim-left "-"))))
