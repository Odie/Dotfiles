(use-package powerline
	:config
	(progn
		(powerline-default-theme)
		))

(use-package helm-config
	:config
	(progn
	  (helm-mode 1)
	  ))

(use-package helm-config
	:config
	(progn
		(when (memq window-system '(mac ns))
  		(exec-path-from-shell-initialize))
	  ))
