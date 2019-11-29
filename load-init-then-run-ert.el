(require 'ert)
(load (expand-file-name "init.el"))
(load (expand-file-name "my-tests.el"))
(ert-run-tests-batch-and-exit)
