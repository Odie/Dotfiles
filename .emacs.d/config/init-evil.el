(setq lexical-binding t)

(use-package evil
	:config
	(progn
	  (evil-mode 1)
		(setq evil-search-module 'evil-search)
		(setq evil-magic 'very-magic)
		(setq evil-emacs-state-cursor '("red" box))
		(setq evil-normal-state-cursor '("green" box))
		(setq evil-visual-state-cursor '("orange" box))
		(setq evil-insert-state-cursor '("red" bar))
		(setq evil-replace-state-cursor '("red" bar))
		(setq evil-operator-state-cursor '("red" hollow))

		; Vertically center selected search results
		(defadvice evil-search-next (after advice-for-evil-ex-search-next activate)
  		(recenter))
		(ad-activate 'evil-search-next)

		(defadvice evil-search-previous (after advice-for-evil-ex-search-previous activate)
  		(recenter))
		(ad-activate 'evil-search-previous)

		; Automatically enable evil-mode after entering these major modes
		(defcustom dotemacs-evil-state-modes
  		'(fundamental-mode
    		text-mode
    		prog-mode
    		sws-mode
    		dired-mode
    		comint-mode
    		log-edit-mode
    		compilation-mode)
  		"List of modes that should start up in Evil state."
  		:type '(repeat (symbol))
  		:group 'dotemacs)

		(defun dotemacs-enable-evil-mode ()
  		(if (apply 'derived-mode-p dotemacs-evil-state-modes)
      		(turn-on-evil-mode)
    		(set-cursor-color "red")))

		(add-hook 'after-change-major-mode-hook 'dotemacs-enable-evil-mode)
	  ))

(setq evilnc-hotkey-comment-operator "gc")
(use-package evil-nerd-commenter
	:config
	(progn
		(evilnc-default-hotkeys)
		))

(use-package evil-indent-textobject)
(use-package evil-visualstar)
(use-package evil-jumper
	:config
	(progn
		(setq evil-jumper-auto-center t)
		; (setq evil-jumper-file (expand-file-name "evil-jump" prelude-savefile-dir))
		(setq evil-jumper-auto-save-interval 3600)
	  ))

(use-package evil-org)


(use-package evil-leader
	:config
	(progn
		(global-evil-leader-mode t)
		(evil-leader/set-leader "<SPC>")
		))

(use-package evil-surround
	:config
	(progn
		(global-evil-surround-mode t)
		))

(use-package evil-exchange
	:config
	(progn
		(evil-exchange-install)
		))

(use-package evil-matchit
	:config
	(progn
		(global-evil-matchit-mode t)
		))

(use-package evil-org)

; Color the mode line according to the mode we are in
(defun my-evil-modeline-change (default-color)
  "changes the modeline color when the evil mode changes"
  (let ((color (cond ((evil-insert-state-p) '("#0087AF" . "#ffffff"))
                     ((evil-visual-state-p) '("#FFB001" . "#875F00"))
                     ((evil-normal-state-p) default-color)
                     (t '("#440000" . "#ffffff")))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

(let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))



(provide 'init-evil)
