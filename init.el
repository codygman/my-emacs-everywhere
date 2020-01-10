(defun my-emacs-everywhere-directory ()
  (if (eq nil (getenv "TRAVIS_OS_NAME"))
      "~/.emacs.d/"
    "~/build/codygman/my-emacs-everywhere/"))
(setq straight-profiles '((nil . "~/.emacs.d/straight-versions.el")))
(setq straight-repository-branch "develop")
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

(setq straight-use-package-by-default t)

(straight-use-package 'org-plus-contrib)
(straight-use-package '(org :local-repo nil))
(straight-use-package 'use-package)

(when (file-exists-p "/data/data/com.termux/files/usr/bin/termux-info")
  (setq straight-use-symlinks nil))

;; make my-emacs-everywhere use literate config
;;;; make my-emacs-everywhere literate config be workflow driven
;;;;; project navigation
;;;;; literate programming
;;;;;;; test that noweb examples work
;;;;;;; test that my library of babel provides things I expect in workflow
;;;;; literate work log
;;;;;;; log command line outputs transparently when :log present (autolog)
;;;;; programming language interaction
;;;;;;; haskell interaction
;;;;;;; nix interaction
;;;;; TBD
;;;;; TBD
;;;;; TBD
;; TODO make my-emacs-everywhere use literate test that gets tangled
(org-babel-load-file (format "%s/emacs-config.org" (my-emacs-everywhere-directory)))


