(package-initialize)

(defun upgrade-and-convey (package)
  (message "Upgrading %s" package)
  (let ((inhibit-message-regexps ".*"))
    (with-no-warnings
      (package-upgrade package)))
  (message "Upgrade complete"))

(defun upgrade-packages ()
  (package-refresh-contents)
  (let ((upgradeable (read (cadr argv))))
    (if (not upgradeable)
        (message "No packages to upgrade")
      (mapc #'upgrade-and-convey upgradeable))))

(upgrade-packages)
