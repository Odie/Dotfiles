;;; Personal settings

; Turn on line numbers
(global-linum-mode t)

; Tell emacs where the theme files live
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;==============================================================================
; Tab settings

; Set default tab-width
(setq-default tab-width 2)

;==============================================================================
; Whitespace settings

; Remove trailing whitespace on file save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables
 ; Turn off whitespace highlighting
 '(prelude-whitespace nil)

 ; Turn on trailing whitespace highlighting
 '(show-trailing-whitespace t))


;==============================================================================
; Better keybindings

; Undo default prelude keychords
(key-chord-define-global "jj" nil)
(key-chord-define-global "jk" nil)
(key-chord-define-global "jl" nil)
(key-chord-define-global "JJ" nil)
(key-chord-define-global "uu" nil)
(key-chord-define-global "xx" nil)
(key-chord-define-global "yy" nil)

; Switch ":" and ";"
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)

; Enter "kj" to escape from insert mode back to normal mode
(define-key evil-insert-state-map (kbd "kj") 'evil-normal-state)

; Use "C-p" for super fast file navigation, aka "goto anything"
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)
