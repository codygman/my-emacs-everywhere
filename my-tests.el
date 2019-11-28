(defun my-emacs-everywhere-directory ()
  (if (eq nil (getenv "TRAVIS_OS_NAME"))
      "~/.emacs.d"
    "~/build/codygman/my-emacs-everywhere/"))

(defun open-lorem-ipsum-goto-end-scroll-up-return-char-position ()
  (save-excursion
    (find-file "testdata/loremipsum.txt")
    (goto-char (point-max))
    (call-interactively 'evil-scroll-up)
    (point)))

;; START evil
(ert-deftest evil-ctrl-u-scrolls-up ()
  (should (eq 4599 (open-lorem-ipsum-goto-end-scroll-up-return-char-position))))
;; END evil
