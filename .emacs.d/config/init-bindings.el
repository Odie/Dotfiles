(defmacro bind (&rest commands)
	"Convience macro which creates a lambda interactive command."
	`(lambda ()
		(interactive)
		,@commands))

(defmacro after (feature &rest body)
	"After FEATURE is loaded, evaluate BODY."
	(declare (indent defun))
	`(eval-after-load ,feature
		'(progn ,@body)))

;==============================================================================
; Evil mode

(after 'evil
	; Switch ":" and ";"
	(define-key evil-normal-state-map (kbd ";") 'evil-ex)
	(define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)

	; Enter "kj" to escape from insert mode back to normal mode
	(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

	; "j" and "k" should move between visual lines instead of actual lines
	(define-key evil-motion-state-map "j" 'evil-next-visual-line)
	(define-key evil-motion-state-map "k" 'evil-previous-visual-line)

	; Quickly switch between buffers
	(define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
	(define-key evil-normal-state-map (kbd "] b") 'next-buffer)

	; Move around windows faster
	(global-set-key (kbd "C-w") 'evil-window-map)
	(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
	(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
	(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
	(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

	(after "helm-projectile-autoloads"
		; Use "C-p" for super fast file navigation, aka "goto anything"
		(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)

		; Various bindings found from https://github.com/bling/dotemacs.git
		;; (define-key evil-normal-state-map (kbd "g b") 'helm-mini)
		;; (define-key evil-normal-state-map (kbd "SPC f") 'helm-find-files)
		;; (define-key evil-normal-state-map (kbd "SPC o") 'helm-imenu)
		;; (define-key evil-normal-state-map (kbd "SPC t") 'helm-etags-select)
		;; (define-key evil-normal-state-map (kbd "SPC y") 'helm-show-kill-ring)
		;; (define-key evil-normal-state-map (kbd "SPC m") 'helm-bookmarks)
		;; (define-key evil-normal-state-map (kbd "SPC r") 'helm-register)))
		)

	(after "evil-leader-autoloads"
		(evil-leader/set-key
			"h" help-map)

		(evil-leader/set-key "/"
			(bind
			 (interactive)
			 (call-interactively (cond ((executable-find "pt")
																	'projectile-pt)
																 ((executable-find "ag")
																	'projectile-ag)
																 ((executable-find "ack")
																	'projectile-ack)
																 (t
																	'projectile-grep)))))))

;==============================================================================
; Org mode

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

(provide 'init-bindings)
;;; init-bindings.el ends here
