(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(straight-use-package 'evil)
(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-want-C-u-scroll t)
  :config
  (setq evil-symbol-word-search t)
  (evil-mode 1)
  )

;; this works fine
;; (call-interactively 'evil-scroll-up)

;; what about my whole function?
(defun open-lorem-ipsum-goto-end-scroll-up-return-char-position ()
  (save-excursion
    (find-file "~/.emacs.d/testdata/loremipsum.txt")
    (message "find-file")
    ;; (message (format "point max is: %s" (point-max)))
    ;; (message (format "point max type is: %s" (type-of (point-max))))
    ;; (message (format "fixnump of point max returns: %s" (fixnump (point-max))))
    (goto-char (point-max))
    (message "goto-char")
    (call-interactively 'evil-scroll-up)
    (message "call-interactively")
    (point))
  )
;; the problems is here :)
(open-lorem-ipsum-goto-end-scroll-up-return-char-position)

;; get this backtrace
;; Debugger entered--Lisp error: (wrong-type-argument fixnump nil)
;;   posn-at-x-y(nil nil)
;;   evil-scroll-up(nil)
;;   funcall-interactively(evil-scroll-up nil)
;;   call-interactively(evil-scroll-up)
;;   (save-excursion (find-file "~/.emacs.d/testdata/loremipsum.txt") (message "find-file") (goto-char (point-max)) (message "goto-char") (call-interactively 'evil-scroll-up) (message "call-interactively") (point))
;;   open-lorem-ipsum-goto-end-scroll-up-return-char-position()
;;   eval-buffer()  ; Reading at buffer position 1455
;;   funcall-interactively(eval-buffer)
;;   call-interactively(eval-buffer record nil)
;;   command-execute(eval-buffer record)
;;   execute-extended-command(nil "eval-buffer" nil)
;;   funcall-interactively(execute-extended-command nil "eval-buffer" nil)
;;   call-interactively(execute-extended-command nil nil)
;;   command-execute(execute-extended-command)

