;;; PersonalSettings --- Personal settings for Odie
;;; Commentary:
;;; Code:

(defun load-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

; Load all files in the elisp and config directory
(load-directory (expand-file-name "elisp" prelude-personal-dir))
(load-directory (expand-file-name "config" prelude-personal-dir))

; Turn on line numbers
; "linum-off.el" deals with turning line numbers off when appropriate
(global-linum-mode t)

; Turn off scrollbar
(scroll-bar-mode -1)

; Tell emacs where the theme files live
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


; Additional settings when we're running in graphical mode
(when (display-graphic-p)

      ;; default Latin font (e.g. Consolas)
      (set-face-attribute 'default nil :family "Anonymous Pro")

      ;; default font size (point * 10)
      (set-face-attribute 'default nil :height 180)

      ;; use specific font for Korean charset.
      ;; if you want to use different font size for specific charset,
      ;; add :size POINT-SIZE in the font-spec.
      (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

      ;; Set the frame to something more reasonable
      (set-frame-size (selected-frame) 125 64)
      )



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
