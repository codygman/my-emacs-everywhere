(ert-deftest version-check ()
  (should (string-equal "27.0.50" emacs-version)))

(ert-deftest straight-el-installed ()
  (should (fboundp 'straight-use-package)))

(ert-deftest org-mode-installed ()
  (should (string-equal "9.2.6" org-version)))

(ert-deftest helm-installed ()
  (should (fboundp 'helm-mode)))

(ert-deftest evil-installed ()
  (should (fboundp 'evil-version)))

(ert-deftest evil-collection-installed ()
  (should (fboundp 'evil-collection-init)))

(ert-deftest avy-installed ()
  (should (fboundp 'avy-goto-char-timer)))

(ert-deftest magit-installed ()
  (should (fboundp 'magit-version)))

(ert-deftest helpful-installed ()
  (should (fboundp 'helpful-callable)))

(ert-deftest helm-swoop-installed ()
  (should (fboundp 'helm-swoop)))

(ert-deftest winner-mode-active ()
  (bound-and-true-p winner-mode))

;; more specific tests
(ert-deftest searching-works-as-expected ()
  (should (not (eq nil (executable-find "rg"))))
  (should (string-equal "rg --color=always --smart-case --no-heading --line-number %s %s %s" helm-grep-ag-command))
  (require 'helm-grep)
  ;; (should (string-equal "helm-do-grep-ag"
  ;; 			(key-binding (kbd "SPC g g"))))
  )

(ert-deftest haskell-mode-enabled-opening-haskell-file ()
  (find-file "~/build/codygman/my-emacs-everywhere/testdata/simple-haskell-project/Main.hs")
  (should (eq 'haskell-mode (derived-mode-p 'haskell-mode))))


 (defun get-substring-from-line ()
      "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
      (interactive)
      (let ((beg (line-beginning-position 1))
            (end (line-beginning-position 2)))
          (string-trim (substring-no-properties (buffer-substring beg end)))))

(defun load-simple-hs-file-and-return-ghci-evald-main ()
  (save-excursion
    (find-file "~/build/codygman/my-emacs-everywhere/testdata/simple-haskell-project/Main.hs")
    (message "found file")
    ;; (find-file "~/source/my-emacs-everywhere/testdata/simple-haskell-project/Main.hs")
    (haskell-process-load-file)
    (message "loaded file")
    (switch-to-buffer "*simple-haskell-project*")
    (message "switched to buffer")
    (message "START ghci repl looks like")
    (message
     (format "%s" (substring-no-properties (buffer-substring (point-min) (point-max)))))
    (message "END ghci repl looks like")
    (goto-char (point-max))
    (message "went to point max")
    (evil-append-line 1)
    (message "insert mode at end of line")
    (insert "main")
    (message "inserted text 'main'")
    (haskell-interactive-mode-return)
    (message "haskell return")
    (message "START ghci repl looks like (after execute)")
    (message
     (format "%s" (substring-no-properties (buffer-substring (point-min) (point-max)))))
    (message "END ghci repl looks like (after execute)")
    (evil-previous-line)
    (message "go to previous line")
    (get-substring-from-line)
    ))

(ert-deftest haskell-mode-ghci-loads-file-and-can-execute ()
  (should (string-equal
	   "Hello, Haskell!"
	   (load-simple-hs-file-and-return-ghci-evald-main))))

;; TODO RET works in grep buffers
;; (with-eval-after-load 'evil-maps
;;   (define-key evil-motion-state-map (kbd "SPC") nil)
;;   (define-key evil-motion-state-map (kbd "RET") nil)
;;   (define-key evil-motion-state-map (kbd "TAB") nil))
;; )

