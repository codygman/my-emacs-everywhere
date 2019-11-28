(defun my-emacs-everywhere-directory ()
  (if (eq nil (getenv "TRAVIS_OS_NAME"))
      "~/.emacs.d"
    "~/build/codygman/my-emacs-everywhere/"))

(defun open-lorem-ipsum-goto-end-scroll-up-return-char-position ()
    (call-interactively 'evil-scroll-up))

;; START evil
(ert-deftest evil-ctrl-u-scrolls-up ()
  (should (eq 4599 (open-lorem-ipsum-goto-end-scroll-up-return-char-position))))
;; END evil
