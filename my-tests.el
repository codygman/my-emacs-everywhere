(ert-deftest version-check ()
  (should (string-equal "27.0.50" emacs-version)))

(ert-deftest straight-el-installed ()
  (should (fboundp 'straight-use-package)))

(ert-deftest org-mode-installed ()
  (should (string-equal "9.2.6" org-version)))
