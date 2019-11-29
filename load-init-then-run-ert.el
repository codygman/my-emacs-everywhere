(require 'ert)
(load (expand-file-name "init.el"))
(load (expand-file-name "my-tests.el"))
(message (expand-file-name "my-tests.el"))

;; credit to https://github.com/emacs-evil/evil/blob/5a7f02c3d5bf3a3feb0db5f0f8175cd5f8136388/evil-tests.el#L119
(defun run-tests-simulating-batch (&optional tests interactive)
  (interactive '(nil t))
  (let ((elp-use-standard-output (not interactive)))
    ;; We would like to use `ert-run-tests-batch-and-exit'
    ;; Unfortunately it doesn't work outside of batch mode, and we
    ;; can't use batch mode because we have tests that need windows.
    ;; Instead, run the tests interactively, copy the results to a
    ;; text file, and then exit with an appropriate code.
    (setq attempt-stack-overflow-recovery nil
	  attempt-orderly-shutdown-on-fatal-signal nil)
    (unwind-protect
	(progn
	  (ert-run-tests-interactively "t")
	  (with-current-buffer "*ert*"
	    (append-to-file (point-min) (point-max) "test-results.txt")
	    (kill-emacs (if (zerop (ert-stats-completed-unexpected ert--results-stats)) 0 1))))
      (unwind-protect
	  (progn
	    (append-to-file "Error running tests\n" nil "test-results.txt")
	    (append-to-file (backtrace-to-string (backtrace-get-frames 'backtrace)) nil "test-results.txt"))
	(kill-emacs 2)))))
