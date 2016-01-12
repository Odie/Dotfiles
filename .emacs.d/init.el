(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

; Tell emacs where the theme files live
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

; Tell emacs which theme we'd like to use
(load-theme 'gruvbox t)

; Turn on line numbers
; "linum-off.el" deals with turning line numbers off when appropriate
(global-linum-mode t)

; Always highlight the current line
(global-hl-line-mode t)

; Turn off scrollbar
(scroll-bar-mode -1)

; Turn off the toolbar
(tool-bar-mode -1)

; Additional settings when we're running in graphical mode
(when (display-graphic-p)

      ;; default Latin font (e.g. Consolas)
      (set-face-attribute 'default nil :family "Anonymous Pro")

    (if (eq (display-pixel-height) 1440)
        (progn
      			;; default font size (point * 10)
            (set-face-attribute 'default nil :height 160)

            ;; Set the frame to something more reasonable
            (set-frame-size (selected-frame) 200 60)

            ;; Place the frame at the top center of the monitor
						(set-frame-position (selected-frame)
						                    (/ (-
						                         (display-pixel-width)
						                         (frame-pixel-width (selected-frame)))
						                       2)
						                    0))

        (progn
            ;; default font size (point * 10)
            (set-face-attribute 'default nil :height 180)

      ;; Set the frame to something more reasonable
      (set-frame-size (selected-frame) 125 64)
          )))


; Load all files in the elisp and config directory
(require 'use-package)

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

(load-directory "~/.emacs.d/elisp")
(load-directory "~/.emacs.d/config")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" default)))
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
