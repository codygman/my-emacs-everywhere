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

;; TODO RET works in grep buffers
;; (with-eval-after-load 'evil-maps
;;   (define-key evil-motion-state-map (kbd "SPC") nil)
;;   (define-key evil-motion-state-map (kbd "RET") nil)
;;   (define-key evil-motion-state-map (kbd "TAB") nil))
;; )

