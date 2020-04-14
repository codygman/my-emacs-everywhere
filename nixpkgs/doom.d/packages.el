(package! eglot)
(after! haskell-mode
  (add-hook 'haskell-mode-hook 'eglot-ensure))
