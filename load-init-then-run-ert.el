(require 'ert)
(load (expand-file-name "emacs-config.el"))
(load (expand-file-name "my-tests.el"))
(ert-run-tests-batch-and-exit)
