;==============================================================================
; Tab settings

; Set default tab-width
(setq-default tab-width 2)
(setq-default smart-tabs-mode t)



;==============================================================================
; Whitespace settings

; Remove trailing whitespace on file save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables
 ; Turn on trailing whitespace highlighting
 '(show-trailing-whitespace t))


;==============================================================================
; Temporary file settings
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

