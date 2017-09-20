;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     asciidoc
     javascript
     yaml
     python
     lua
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-enable-snippets-in-popup t)
     ;; better-defaults
     emacs-lisp
     (clojure :variables
              clojure-enable-fancify-symbols t
              nrepl-buffer-name-show-port t)
     octave
     html
     git
     (markdown :variables markdown-live-preview-engine 'vmd)
     org
     sql
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     osx
     spell-checking
     syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(auto-dim-other-buffers inf-clojure)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(gruvbox
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Anonymous Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.4)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ";"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers 'relative
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   )

  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (add-to-list 'configuration-layer--elpa-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
  (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(ac-cider . "melpa-stable") t)
  (user/spacemacs-init)
  )

(defmacro after (feature &rest body)
	"After FEATURE is loaded, evaluate BODY."
	(declare (indent defun))
	`(eval-after-load ,feature
     '(progn ,@body)))

(defun user/display-type ()
    (let ((width (display-pixel-width))
          (height (display-pixel-height)))

      (if (and (eq width 1440)
               (eq height 900))
               'MBP
        'other)
      ))

(defvar user/display-configs
  '(
    ((config-name . workstation)
     (resolution . (3440 . 1440))
     (frame-size . (213 . 99))
     (frame-position . top-center))

    ((config-name . MBP)
     (resolution . (1440 . 900))
     (frame-size . (125 . 61))
     (frame-position . top-center))))

(defun user/match-display-config (display-configs)
  "Given a list of display configs, retrieve the one that matches the current display or nil"

  ;; What is the current width & height of the current display?
  (let ((cur-resolution (cons (display-pixel-width) (display-pixel-height)))
        (result))

    ;; Loop through the configs and find a match
    (dolist (config display-configs)

      ;; Grab the item that matches the current display
      (if (equal cur-resolution (alist-get 'resolution config))
          (setq result config)))

    ;; Return the found item
    result))

(defun cons->list (c)
  (list (car c) (cdr c)))

(defun user/config-frame-for-display (display-config)
  "Given a display config, update the current frame's size and position to match"
  (let ((frame-size (or (alist-get 'frame-size display-config) (cons 100 10)))
        (frame-pos (or (alist-get 'frame-position display-config) (cons 0 0))))

    (apply 'set-frame-size (selected-frame) (cons->list frame-size))

    (cond ((consp frame-pos)
           (apply 'set-frame-position (selected-frame) (cons->list frame-pos)))

          ((equal frame-pos 'top-center)
           (set-frame-position (selected-frame) (center-frame-x) 0)))))

(defun user/spacemacs-init ()
  (setq dotspacemacs-default-font
        (cond
         ((eq (user/display-type) 'MBP)
          '("Anonymous Pro"
            :size 13
            :weight normal
            :width normal
            :powerline-scale 1.4))

         (:else
          '("Anonymous Pro"
            :size 14
            :weight normal
            :width normal
            :powerline-scale 1.4)))))

(defun center-frame-x ()
  "Get the horizontal position where the frame should be centered"
  (/ (- (display-pixel-width) (frame-pixel-width)) 2))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun user/scroll-line-to-center (orig-fun &rest args)
  (call-interactively 'evil-scroll-line-to-center))

(defun user/mode-active? (mode)
  (or
   (eq major-mode mode)
   (and (symbolp mode) (boundp mode) (symbol-value mode))))

(defun user/clj-eval-last-sexp (&optional no-state)
  "Evaluate the last expression depending on what clojure mode is active"
  (interactive)

  (if (user/mode-active? 'inf-clojure-minor-mode)
      (inf-clojure-eval-last-sexp)
    (cider-eval-last-sexp)))

(defun user/clj-eval-buffer (&optional no-state)
  "Evaluate the last expression depending on what clojure mode is active"
  (interactive)

  (if (user/mode-active? 'inf-clojure-minor-mode)
      (inf-clojure-eval-buffer)
    (cider-eval-buffer)))

(defun user/clj-eval-defun-at-point (&optional no-state)
  "Evaluate the last expression depending on what clojure mode is active"
  (interactive)

  (if (user/mode-active? 'inf-clojure-minor-mode)
      (inf-clojure-eval-defun)
    (cider-eval-defun-at-point)))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; Temp fix for helm-book-map error
  (require 'helm-bookmark)

  ;;;---------------------------------------------------------------------------
  ;;; Configure startup frame size
  ;;;

  ;; If we're using a graphical display, update the frame of the emacs application
  ;; so it's a reasonable size for the display as well as centered.
  (when (display-graphic-p)
    ;; Grab a config suitable for the current display
    (let ((display-config (or (user/match-display-config user/display-configs)
                              '((config-name . default)
                                (frame-size . (100 . 10))
                                (frame-position . (0 . 0))))))

      ;; Apply the said config
      (user/config-frame-for-display display-config)

      ;; Perform any other custom behavior not handled by config-frame-for-display
      (when (eq (alist-get 'config-name display-config) 'MBP)
        ;; Go fullscreen on a MBP. There is otherwise too little room to work.
        (spacemacs/toggle-fullscreen-frame))))

  ;;;---------------------------------------------------------------------------
  ;;; Global editor behaviors
  ;;;

  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none)

  (use-package evil
    :config
    ;; Switch ":" and ";"
    (define-key evil-normal-state-map (kbd ";") 'evil-ex)
    (define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)

    ;; "j" and "k" should move between visual lines instead of actual lines
    (define-key evil-motion-state-map "j" 'evil-next-visual-line)
    (define-key evil-motion-state-map "k" 'evil-previous-visual-line)

    ;; Enter "kj" to escape from insert mode back to normal mode
    (setq-default evil-escape-key-sequence "kj")

    ;; Emulate ctrl-p
    (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)

    ;; Move around windows faster
    (global-set-key (kbd "C-h") 'evil-window-left)
    (global-set-key (kbd "C-j") 'evil-window-down)
    (global-set-key (kbd "C-k") 'evil-window-up)
    (global-set-key (kbd "C-l") 'evil-window-right)

    (advice-add 'evil-goto-mark-line :after #'user/scroll-line-to-center))

  ;; Don't ask about symbolic links that links to version controlled files
  ;; More specifically, the .spacemacs file is under source control. Without this
  ;; option, emacs would ask whether to follow the link each time the spacemacs
  ;; file is opened.
  ;;
  ;; Since we're almost always working with git, it does not matter if we just start
  ;; modifying the file without any sort of vc specific locking operation.
  (setq vc-follow-symlinks t)

  ;; Always delete trailing whitespace
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Automatically dim buffers that are not in focus
  (use-package auto-dim-other-buffers
    :ensure t
    :init
    (auto-dim-other-buffers-mode t)
    :config
    (with-eval-after-load 'diminish
      (diminish 'auto-dim-other-buffers-mode)))

  ;;;---------------------------------------------------------------------------
  ;;; ORG mode
  ;;;

  ;; Define where the org agenda files are
  ;; These files are used by org-mode to find where the TODO items are
  ;; Without this setting, org-mode agenda will always show an empty agenda
  ;; because there are no known TODO items.
  (use-package org
    :init
    ;; (add-hook 'org-mode-hook 'org-indent-mode t)

    :config
    (setq org-startup-truncated nil)
    (setq org-startup-indented t)
    (setq org-agenda-files (list "~/org/" "~/org/projects/"))
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "it" 'org-inlinetask-insert-task))

  (use-package org-agenda
    :config
    (evil-define-key 'evilified org-agenda-mode-map
      (kbd "C-h") 'evil-window-left
      (kbd "C-j") 'evil-window-down
      (kbd "C-k") 'evil-window-up
      (kbd "C-l") 'evil-window-right))

  (require 'org-inlinetask)  ;; allow inline todos
  (add-hook 'org-mode-hook 'toggle-word-wrap) ;; enable soft-wrapping at word boundaries.


  ;;;---------------------------------------------------------------------------
  ;;; Cider
  ;;;

  (use-package cider
    :init
    (setq cider-repl-history-file "~/.cider-history")
    (setq cider-repl-use-pretty-printing t))

  ;; Turn on docstring popup when scrolling through the autocomplete window
  (add-hook 'clojure-mode-hook 'company-quickhelp-mode)
  (setq company-quickhelp-delay 0.1)

  ;; Fix C-k to scroll up
  ;; For some reason, enabling quickhelp mode breaks the C-k keybinding
  (add-hook
   'company-completion-started-hook
   (lambda (&rest ignore)
     (when evil-mode
       (when (evil-insert-state-p)
         (define-key evil-insert-state-map (kbd "C-k") nil)))))

  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
    "ee" 'user/clj-eval-last-sexp)
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
    "eb" 'user/clj-eval-buffer)
  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
    "ef" 'user/clj-eval-defun-at-point)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-pprint-fn (quote fipp))
 '(org-agenda-custom-commands
   (quote
    (("o" "Overdue todo items" tags "+SCHEDULED<\"<now>\"&-activity" nil)
     ("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil))))
 '(package-selected-packages
   (quote
    (sql-indent yaml-mode python-x folding yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic winum fuzzy lua-mode parinfer smeargle orgit magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit with-editor auto-dim-other-buffers smooth-scroll pug-mode hide-comnt inflections edn multiple-cursors paredit peg cider seq queue clojure-mode powerline spinner org markdown-mode hydra parent-mode projectile pos-tip flycheck pkg-info epl flx smartparens iedit anzu evil goto-chg undo-tree eval-sexp-fu highlight s diminish bind-map bind-key yasnippet packed dash helm avy helm-core async popup package-build company auto-complete uuidgen request osx-dictionary org-projectile org-download link-hint flyspell-correct-helm flyspell-correct eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff dumb-jump f column-enforce-mode clojure-snippets web-mode tagedit slim-mode scss-mode sass-mode less-css-mode jade-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data toc-org reveal-in-osx-finder pbcopy osx-trash org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-bullets launchctl htmlize gnuplot ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe use-package spacemacs-theme spaceline smooth-scrolling restart-emacs rainbow-delimiters quelpa popwin persp-mode pcre2el paradox page-break-lines open-junk-file neotree move-text mmm-mode markdown-toc macrostep lorem-ipsum linum-relative leuven-theme info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flyspell helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag gruvbox-theme google-translate golden-ratio gh-md flycheck-pos-tip flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu elisp-slime-nav define-word company-statistics company-quickhelp clj-refactor clean-aindent-mode cider-eval-sexp-fu buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#282828" :foreground "#fdf4c1"))))
 '(auto-dim-other-buffers-face ((t (:background "gray23"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(cider-pprint-fn (quote fipp))
 '(evil-want-Y-yank-to-eol nil)
 '(inf-clojure-generic-cmd "lumo -d")
 '(js2-strict-missing-semi-warning nil)
 '(org-agenda-custom-commands
   (quote
    (("o" "Overdue todo items" tags "+SCHEDULED<\"<now>\"&-activity" nil)
     ("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil))))
 '(package-selected-packages
   (quote
    (adoc-mode markup-faces sql-indent yaml-mode python-x folding yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic winum fuzzy lua-mode parinfer smeargle orgit magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit with-editor auto-dim-other-buffers smooth-scroll pug-mode hide-comnt inflections edn multiple-cursors paredit peg cider seq queue clojure-mode powerline spinner org markdown-mode hydra parent-mode projectile pos-tip flycheck pkg-info epl flx smartparens iedit anzu evil goto-chg undo-tree eval-sexp-fu highlight s diminish bind-map bind-key yasnippet packed dash helm avy helm-core async popup package-build company auto-complete uuidgen request osx-dictionary org-projectile org-download link-hint flyspell-correct-helm flyspell-correct eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff dumb-jump f column-enforce-mode clojure-snippets web-mode tagedit slim-mode scss-mode sass-mode less-css-mode jade-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data toc-org reveal-in-osx-finder pbcopy osx-trash org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-bullets launchctl htmlize gnuplot ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe use-package spacemacs-theme spaceline smooth-scrolling restart-emacs rainbow-delimiters quelpa popwin persp-mode pcre2el paradox page-break-lines open-junk-file neotree move-text mmm-mode markdown-toc macrostep lorem-ipsum linum-relative leuven-theme info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flyspell helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag gruvbox-theme google-translate golden-ratio gh-md flycheck-pos-tip flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu elisp-slime-nav define-word company-statistics company-quickhelp clj-refactor clean-aindent-mode cider-eval-sexp-fu buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(safe-local-variable-values
   (quote
    ((js2-missing-semi-one-line-override)
     (js2-strict-missing-semi-warning)
     (inf-clojure-boot-cmd . "lumo -d")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#282828" :foreground "#fdf4c1"))))
 '(auto-dim-other-buffers-face ((t (:background "gray23"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(markup-list-face ((t (:inherit markup-meta-face :foreground "plum1"))))
 '(markup-meta-face ((t (:stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal))))
 '(markup-meta-hide-face ((t (:inherit markup-meta-face))))
 '(markup-secondary-text-face ((t (:inherit markup-gen-face :foreground "firebrick"))))
 '(markup-title-0-face ((t (:inherit markup-gen-face :weight bold))))
 '(markup-title-1-face ((t (:inherit markup-gen-face :weight bold))))
 '(markup-title-2-face ((t (:inherit markup-gen-face :weight bold))))
 '(markup-title-3-face ((t (:inherit markup-gen-face :weight bold))))
 '(markup-title-4-face ((t (:inherit markup-gen-face :weight bold))))
 '(markup-title-5-face ((t (:inherit markup-gen-face :weight bold))))
 '(markup-value-face ((t nil)))
 '(markup-verbatim-face ((t nil))))
)
