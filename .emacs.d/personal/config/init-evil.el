(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(setq evilnc-hotkey-comment-operator "gc")

(setq evil-jumper-auto-center t)
(setq evil-jumper-file (expand-file-name "evil-jump" prelude-savefile-dir))
(setq evil-jumper-auto-save-interval 3600)

(prelude-require-package 'evil)
(prelude-require-package 'evil-leader)
(prelude-require-package 'evil-numbers)
(prelude-require-package 'evil-visualstar)
(prelude-require-package 'evil-nerd-commenter)
(prelude-require-package 'evil-indent-textobject)
(prelude-require-package 'evil-matchit)
(prelude-require-package 'evil-exchange)
(prelude-require-package 'evil-surround)
(prelude-require-package 'evil-jumper)
(prelude-require-package 'evil-search-highlight-persist)

(require 'evil)
(require 'evil-nerd-commenter)
(require 'evil-indent-textobject)
(require 'evil-visualstar)
(require 'evil-jumper)

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

(global-evil-leader-mode t)
(evil-leader/set-leader ",")

(global-evil-surround-mode t)
(evil-exchange-install)

(defun evilmi-customize-keybinding ()
  (evil-define-key 'normal evil-matchit-mode-map
    "%" 'evilmi-jump-items))
(global-evil-matchit-mode t)

(defun my-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))

(defun my-evil-terminal-cursor-change ()
  (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
  (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

(add-hook 'after-make-frame-functions (lambda (frame) (my-evil-terminal-cursor-change)))
(my-evil-terminal-cursor-change)



; Color the mode line according to the mode we are in
(defun my-evil-modeline-change (default-color)
  "changes the modeline color when the evil mode changes"
  (let ((color (cond ((evil-insert-state-p) '("#002233" . "#ffffff"))
                     ((evil-visual-state-p) '("#330022" . "#ffffff"))
                     ((evil-normal-state-p) default-color)
                     (t '("#440000" . "#ffffff")))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))



; Vertically center selected search results
(defadvice evil-search-next (after advice-for-evil-ex-search-next activate)
  (recenter))
(ad-activate 'evil-search-next)

(defadvice evil-search-previous (after advice-for-evil-ex-search-previous activate)
  (recenter))
(ad-activate 'evil-search-previous)

(provide 'init-evil)
