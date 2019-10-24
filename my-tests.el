(ert-deftest version-check ()
  (should (string-equal "27.0.50" emacs-version)))

(ert-deftest straight-el-installed ()
  (should (fboundp 'straight-use-package)))
