(defun tests-run ()
  "Run Evil tests."
  (interactive '(nil t))
  ;; We would like to use `ert-run-tests-batch-and-exit'
  ;; Unfortunately it doesn't work outside of batch mode, and we
  ;; can't use batch mode because we have tests that need windows.
  ;; Instead, run the tests interactively, copy the results to a
  ;; text file, and then exit with an appropriate code.
  (setq attempt-stack-overflow-recovery nil
        attempt-orderly-shutdown-on-fatal-signal nil)
  (unwind-protect
      (progn
        (ert-run-tests-interactively t)
        (with-current-buffer "*ert*"
          (append-to-file (point-min) (point-max) "test-results.txt")
          (kill-emacs (if (zerop (ert-stats-completed-unexpected ert--results-stats)) 0 1))))
    (unwind-protect
        (progn
          (append-to-file "Error running tests\n" nil "test-results.txt")
          (append-to-file (backtrace-to-string (backtrace-get-frames 'backtrace)) nil "test-results.txt"))
      (kill-emacs 2))))

;; duplicate in init.el
(defun my-emacs-everywhere-directory ()
  (if (eq nil (getenv "TRAVIS_OS_NAME"))
      "~/.emacs.d/"
    "~/build/codygman/my-emacs-everywhere/"))

(ert-deftest version-check ()
  (should (string-equal "27.0.50" emacs-version)))

(ert-deftest straight-el-installed ()
  (should (fboundp 'straight-use-package)))

(ert-deftest org-mode-installed ()
  (should (string-equal "9.2.6" org-version)))

(ert-deftest helm-installed ()
  (should (fboundp 'helm-mode)))

(ert-deftest trivial-helm-example-works-as-expected ()
  (should (string-equal
	   "john"
	   (with-simulated-input
	       '("jo"
		 (wsi-simulate-idle-time 0.5)
		 "RET")
	     (car (helm :sources
			`((name . "Simple helm names example")
			  (candidates . ,(list "jane" "john"))
			  (action . (lambda (candidate) (helm-marked-candidates))))))))))
