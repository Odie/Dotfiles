(use-package clojure-mode
	:mode (("\\.boot\\'" . clojure-mode))

	:config
	(progn
		(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
		(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)))
