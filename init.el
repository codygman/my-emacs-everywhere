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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f8067b7d0dbffb29a79e0843797efabdf5e1cf326639874d8b407e9b034136a4" "97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b" "9129c2759b8ba8e8396fe92535449de3e7ba61fd34569a488dd64e80f5041c9f" default))
 '(notmuch-saved-searches
   '((:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "inbox" :query "tag:inbox"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(notmuch-message-summary-face ((((class color) (min-colors 89)) (:inherit highlight))))
 '(org-special-keyword ((t (:weight thin :foreground "#586e75"))))
 '(org-tag ((t (:weight thin :foreground "#586e75")))))
