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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; BEGIN hacky install org mode
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))


(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)
(straight-use-package 'org-plus-contrib) ; or org-plus-contrib if desired
;; END hacky install org mode

(use-package org
  :straight org-plus-contrib)

(use-package helm
  :init
  (setq helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  :config
  (helm-mode 1)
  (use-package helm-swoop))

(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil)
  :config
  (setq evil-symbol-word-search t)
  (evil-mode 1)
  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init)
    ))

(use-package general
  :config
  (general-evil-setup)
  (general-imap "j"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      ;; TODO make this work so jf writes the file when I enter normal mode
      ;; "f" '(my-write-then-normal-state)
      "f" 'evil-normal-state))
  (general-create-definer my-leader-def
    :prefix "C")
  (my-leader-def
    :states '(normal visual emacs motion)
    :prefix "SPC"
    :keymaps 'override
    :non-normal-prefix "M-SPC"
    "SPC" '(helm-M-x :which-key "M-x")
    "ad"  '(dired :which-key "open dired")
    "ae"  '(eshell :which-key "open eshell")
    "at"  '(shell :which-key "open terminal")
    "bb"  '(helm-mini :which-key "buffers list")
    "bd"  '(spacemacs/kill-this-buffer :which-key "kill-this-buffer")
    "cl" '(comment-line :which-key "comment line")
    "eb"  '(ediff-buffers :which-key "ediff buffers")
    "fed" '(find-dotfile :which-key "go to init.el")
    "ff"  '(helm-find-files :which-key "find files")
    "gb" '(magit-blame :which-key "magit blame")
    "gf" '(magit-find-file :which-key "magit find-file")
    "gg" '(helm-do-grep-ag :which-key "helm ag (rg)")
    "gl" '(magit-log-buffer-file :which-key "magit log file")
    "gs" '(magit-status :which-key "magit status")
    "gt" '(magit-log-trace-definition :which-key "magit trace definition")
    "hdd" '(helm-apropos :which-key "apropos at point")
    "hdf" '(helpful-callable :which-key "describe function")
    "hdk" '(helpful-key :which-key "describe key")
    "hdm" '(describe-mode :which-mode "describe mode")
    "hdv" '(helpful-variable :which-key "describe variable")
    "hr" '(helm-resume :which-mode "helm resume")
    "ji"  '(avy-goto-char-in-line :which-key "Jump To Char In Line")
    "jj"  '(avy-goto-char-timer :which-key "Jump To Char")
    "jl"  '(avy-goto-line :which-key "Jump To line")
    "l"  '(tab-bar-select-tab :which-key "switch perspective")
    "oa"  '(org-agenda-list :which-key "open org agenda list") ;; previously aoa
    "ocj"  '(org-clock-goto :which-key "jump to current clock")
    "ocl"  '(org-clock-in-last :which-key "clock in last task")
    "od"  '(my-day-org-agenda :which-key "open todays org agenda")
    "ol"  '(org-store-link :which-key "store org link") ;; previously aol
    "oo"  '(org-agenda :which-key "open org agenda") ;; previously aoo
    "pb"  '(helm-projectile-switch-to-buffer :which-key "switch buffer")
    "pf"  '(helm-projectile-find-file :which-key "find files")
    "pp"  '(helm-projectile-switch-project :which-key "switch project")
    "pr"  '(helm-show-kill-ring :which-key "show kill ring")
    "qq"  '(save-buffers-kill-emacs :which-key "quit")
    "sS"  '(helm-swoop :which-key "helm-swoop")
    "ss"  '(spacemacs/helm-swoop-region-or-symbol :which-key "helm-swoop-region-or-symbol")
    "tl" '(toggle-truncate-lines :which-key "truncate lines")
    "tw" '(whitespace-mode :which-key "show whitespace")
    "u"   '(universal-argument :which-key "universal-argument")
    "w-"  '(split-window-below :which-key "split bottom")
    "w/"  '(split-window-right :which-key "split right")
    "wd"  '(delete-window :which-key "delete window")
    "wh"  '(evil-window-move-far-left :which-key "move left")
    "wj"  '(evil-window-move-very-bottom :which-key "move bottom")
    "wk"  '(evil-window-move-very-top :which-key "move up")
    "wl"  '(evil-window-move-far-right :which-key "move right")
    "wm"  '(toggle-maximize-buffer :which-key "maximize buffer")
    "wr"  '(winner-redo :which-key "winner redo")
    "wu"  '(winner-undo :which-key "winner undo")
    "wx"  '(delete-window :which-key "delete window")
    )
  )

(use-package avy)

(use-package magit)

(use-package winner
  :requires init-general
  :demand t
  :init
  (setq winner-dont-bind-my-keys t)
  :config
  (winner-mode 1))

(use-package helpful)

;; extras
(defun spacemacs/kill-this-buffer (&optional arg)
    "Kill the current buffer.
            If the universal prefix argument is used then kill also the window."
    (interactive "P")
    (if (window-minibuffer-p)
        (abort-recursive-edit)
      (if (equal '(4) arg)
          (kill-buffer-and-window)
        (kill-buffer))))

(defun my-day-org-agenda ()
      (interactive)
      (let ((org-agenda-span 'day))
        (org-agenda nil "a")
        (progn (switch-to-buffer "*Org Agenda*") (delete-other-windows))
        ))
(defun spacemacs/helm-swoop-region-or-symbol ()
    "Call `helm-swoop' with default input."
    (interactive)
    (let ((helm-swoop-pre-input-function
           (lambda ()
             (if (region-active-p)
                 (buffer-substring-no-properties (region-beginning)
                                                 (region-end))
               (let ((thing (thing-at-point 'symbol t)))
                 (if thing thing ""))))))
      (call-interactively 'helm-swoop))) 

(defun toggle-maximize-buffer () "Maximize buffer"
         (interactive)
         (if (= 1 (length (window-list)))
             (jump-to-register '_)
           (progn
             (window-configuration-to-register '_)
             (delete-other-windows))))
