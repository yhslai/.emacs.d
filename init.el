(set-language-environment "UTF-8")

;; Unset C-z so we don't accidentally hide Emacs
(global-unset-key (kbd "C-z"))

(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway

;; Start with maximized window by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Always show keystrokes in echo area immediately
(setq echo-keystrokes 0.01)

;; Allow installing packages from MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)

;; Set up a custom-file so customization variables (e.g. package-selected-packaged used by Package.el)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Split vertically if there is enough space available
(setq split-width-threshold 160)


;; A few more useful configurations... (recommened by Vertico and Corfu)
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 5)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   emacs-bedrock: ttps://git.sr.ht/~ashton314/emacs-bedrock/tree/main/item/init.el
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq inhibit-splash-screen t)

(setq initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setq display-time-default-load-average nil) ; this information is useless for most


(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; save history of minibuffer
(savehist-mode)

;; move through windows with ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; you can use other modifiers here

;; fix archaic defaults
(setq sentence-end-double-space nil)

;; make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "return a new file path of a given file path.
if the new path's directories does not exist, create them."
  (let* ((backuprootdir "~/.emacs.d/emacs-backup/")
         (filepath (replace-regexp-in-string "[a-za-z]:" "" fpath )) ; remove windows driver letter in path
         (backupfilepath (replace-regexp-in-string "//" "/" (concat backuprootdir filepath "~") )))
    (make-directory (file-name-directory backupfilepath) (file-name-directory backupfilepath))
    backupfilepath))
(setq make-backup-file-name-function 'bedrock--backup-file-name)


;;;
;;;   discovery aids
;;;

;; show the help buffer after startup
;; (add-hook 'after-init-hook 'help-quick)

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. c-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;;;
;;;   minibuffer/completion settings
;;;

;; for help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setq enable-recursive-minibuffers t)                ; use the minibuffer whilst in the minibuffer
(setq completion-cycle-threshold 1)                  ; tab cycles candidates
(setq completions-detailed t)                        ; show annotations
(setq tab-always-indent 'complete)                   ; when i hit tab, try to complete, otherwise, indent
(setq completion-styles '(basic initials substring)) ; different styles to match input to candidates

(setq completion-auto-help 'always)                  ; open completion always; `lazy' another option
(setq completions-max-height 20)                     ; this is arbitrary
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-group t)
(setq completion-auto-select 'second-tab)            ; much more eager
;(setq completion-auto-select t)                     ; see `c-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; tab acts more like how it does in the shell

;; for a fancier built-in completion option, try ido-mode or fido-mode. see also
;; the file mixins/base.el
;(fido-vertical-mode)
;(setq icomplete-delay-completions-threshold 4000)

;;;
;;;   interface enhancements/defaults
;;;

;; mode line information
(setq line-number-mode t)                        ; show current line in modeline
(setq column-number-mode t)                      ; show column as well

(setq x-underline-at-descent-line nil)           ; prettier underlines
(setq switch-to-buffer-obey-display-actions t)   ; make switching buffers more consistent

(setq-default show-trailing-whitespace nil)      ; by default, don't underline trailing spaces
(setq-default indicate-buffer-boundaries 'left)  ; show buffer top and bottom in the margin

;; enable horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; we won't set these, but they're good to know about
;;
;; (setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; misc. ui tweaks
(blink-cursor-mode -1)                                ; steady cursor
(pixel-scroll-precision-mode)                         ; smooth scrolling

;; use common keystrokes by default
(cua-mode)

;; display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width 4)           ; set a minimum width

;; ;; modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))


;;;=======================================================
;;;
;;;   tab-bar configuration
;;;

;; show the tab-bar as soon as tab-bar functions are invoked
(setq tab-bar-show 0)

;; add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setq display-time-format "%a %f %t")
(setq display-time-interval 1)
(display-time-mode)


;;;=======================================================
;;;
;;;   theme
;;;

(use-package emacs
  :config
  (load-theme 'modus-vivendi))          ; for light theme, use modus-operandi


;;;=======================================================
;;;
;;;   evil mode
;;;


;; enable evil-mode by default
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (evil-mode 1)
  (modify-syntax-entry ?_ "w")
  )

;; w / b jump between symbols (including _-) instead of words
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
;; (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop) ; Comment out to use default tab behavior


(defun switch-and-zoom ()
  "Switch to the other window, then zoom."
  (interactive)
  (other-window 1)
  (zoom))     ; Assume here that `zoom` is available 

(global-set-key (kbd "C-o") 'switch-and-zoom)
(define-key evil-normal-state-map (kbd "C-o") 'switch-and-zoom)
(define-key evil-insert-state-map (kbd "C-o") 'switch-and-zoom)
(define-key evil-visual-state-map (kbd "C-o") 'switch-and-zoom)


(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (setq-default undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))


(use-package evil-owl
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))

;;; ======================================================
;;;
;;;   Magit
;;;

(use-package magit
    :bind
    (:map magit-file-section-map
	  ("RET" . magit-diff-visit-file-other-window)
	  :map magit-hunk-section-map
	  ("RET" . magit-diff-visit-file-other-window))
    :config
    (setq magit-define-global-key-bindings 'recommended)
    )


;;; ======================================================
;;;
;;;   Org Mode(s)
;;;

(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

(use-package visual-fill-column
  :ensure t
  :config
  (setq-default visual-fill-column-width 110)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure t
  :config
  (progn
    ;; config stuff
    ))

(use-package org-ai
  :load-path (lambda () (expand-file-name "local-packages/org-ai" user-emacs-directory))
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets) ; if you are using yasnippet and want `ai` snippets
  (add-hook 'org-mode-hook 'display-line-numbers-mode))
  


;; We define <C-j> as <C-x> for easier finger pose, so let's rebind these
(define-key org-mode-map (kbd "<normal-state> C-j") nil)
(define-key org-mode-map (kbd "<normal-state> C-k") nil)
(define-key org-mode-map (kbd "C-j") nil)
(define-key org-mode-map (kbd "C-k") nil)
(define-key org-mode-map (kbd "<normal-state> C-M-j") 'org-forward-heading-same-level)
(define-key org-mode-map (kbd "<normal-state> C-M-k") 'org-backward-heading-same-level)
(define-key org-mode-map (kbd "C-M-j") 'org-forward-heading-same-level)
(define-key org-mode-map (kbd "C-M-k") 'org-backward-heading-same-level)

;; Slightly weird but we switch M-j and M-x too
(define-key org-mode-map (kbd "<normal-state> M-j") nil)
(define-key org-mode-map (kbd "<normal-state> M-x") 'outline-move-subtree-down)

;; Org agenda
(define-key org-mode-map (kbd "C-c a") 'org-agenda)


;;; ======================================================
;;;
;;;   Secret Keys
;;;

(load (concat
	   (file-name-directory user-init-file)
	   "secrets.el"))



;;; ======================================================
;;;
;;;   Misc functions and minor modes
;;;

(recentf-mode 1)

(defun open-all-recent-files ()
    "Open all recent files."
    (interactive)
    (dolist (file recentf-list) (find-file file)))

(use-package desktop
  :init
  (setq desktop-dirname             "~/.emacs.d/desktop/"
		desktop-base-file-name      "emacs.desktop"
		desktop-base-lock-name      "lock"
		desktop-path                (list desktop-dirname)
		desktop-save                t
		desktop-load-locked-desktop nil
		desktop-auto-save-timeout   30
		;; desktop-restore-frames      t
		;; lazy reloading is problematic, preventing Emacs from properly redraw itself
		desktop-restore-eager       t
		)
  :config
  (desktop-save-mode 1)
  (add-to-list 'desktop-modes-not-to-save 'magit-status-mode)
  ;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  ;; (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  ;; (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
)




;;; =======================================================
;;;
;;;   My own shortcuts
;;;

;;; !!!!!! IMPORTANT !!!!!!
;; default <C-j> was newline-and-indent, which has been replaced by evil
;; So we use it as an easier-to-press <C-x> alternative
(global-set-key (kbd "C-j") ctl-x-map)

;; <M-j> as <M-x>
(global-set-key (kbd "M-j") 'execute-extended-command)

;; Removed conflicted keys in eshell mode
;; (define-key eshell-mode-map (kbd "<normal-state> C-j") nil)
;; (define-key eshell-mode-map (kbd "<visual-state> C-j") nil)

(defun split-window-below-and-focus ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-focus ()
  (interactive)
  (split-window-right)
  (other-window 1))

(global-unset-key (kbd "C-x @"))
(global-unset-key (kbd "C-x #"))
(global-set-key (kbd "C-x @") 'split-window-below-and-focus)
(global-set-key (kbd "C-x #") 'split-window-right-and-focus)

;; Default is `list-buffers, but `buffer-menu opens in the current window and I prefer it
(global-set-key (kbd "C-x C-b") 'buffer-menu)


;; Don't clutter the current directory with autosaves and backups
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "autosaves")))))

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "autosaves/") t)))


;;; =======================================================
;;;
;;;   My Own Theme
;;;

(set-face-attribute 'default nil :font "fira code-12")
(set-face-attribute 'mode-line nil :font "fira code-10")
(set-face-attribute 'mode-line-inactive nil :font "fira code-10")
(setq-default line-spacing 3)


;;; =======================================================
;;;
;;;   Spell Checking
;;;

(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
)

(setq languagetool-java-arguments '("-Dfile.encoding=UTF-8"))
(setq languagetool-console-command "~/languagetool/languagetool-commandline.jar")
(setq languagetool-server-command "~/languagetool/languagetool-server.jar")

;; (cond
;;  ((string-equal system-type "windows-nt")
;;   (progn
;; 	(setq ispell-program-name
;; 		(concat (getenv "PROGRAMFILES") "/hunspell/bin/hunspell.exe"))
;; 	))
;;  )

;; (use-package flyspell
;;   :ensure t
;;   :init
;;   (add-hook 'org-mode-hook
;;             (lambda () (flyspell-mode 1))))



;;; =======================================================
;;;
;;;   General Dev Tools
;;;

(use-package eglot
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'eglot-ensure)
  (add-hook 'prog-mode-hook
			(lambda () (local-set-key (kbd "<f2>") #'eglot-rename)))
  (add-hook 'prog-mode-hook
			(lambda () (local-set-key (kbd "M-<f2>") #'flymake-goto-next-error)))
  ;; (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  )

(setq flymake-no-changes-timeout 0.2)

;; Remember to put compiled tree-sitter libraries in ~/.emacs.d/tree-sitter
(use-package tree-sitter
  :ensure t
  )


;; Enable vertico
(use-package vertico
  :init

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  :config
  (vertico-mode)
  )


(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :bind
  (:map corfu-map
		("TAB" . corfu-next)
		([tab] . corfu-next)
		("S-TAB" . corfu-previous)
		([backtab] . corfu-previous)
		)

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `global-corfu-modes'.
  :init
  (global-corfu-mode))


;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
			  ("M-p" . projectile-command-map)
			  ("C-c p" . projectile-command-map)))

(use-package ripgrep
  :load-path (lambda() (expand-file-name "local-packages/ripgrep.el" user-emacs-directory))
  :ensure t)

(use-package projectile-ripgrep
  :load-path (lambda() (expand-file-name "local-packages/ripgrep.el" user-emacs-directory))
  :ensure t)


(use-package string-inflection
  :ensure t)


(defun zoom-size-callback ()
  (cond ((> (frame-pixel-width) 1280) '(115 . 0.75))
        (t                            '(0.5 . 0.5))))

(use-package zoom
  :ensure t
  :init
  (setq zoom-size 'zoom-size-callback)
  (setq zoom-ignored-major-modes '(dired-mode magit-mode)
		zoom-ignored-buffer-names '("zoom.el" "init.e")
		zoom-ignored-buffer-name-regexps '("^*calc")
		)
  :config
  ;; (zoom-mode t) ; Quite buggy...
  )

;; C-c q to undo window change (e.g. dismiss a pop-up)
(setq winner-dont-bind-my-keys t)
(winner-mode)
(define-key winner-mode-map (kbd "C-c q") #'winner-undo)
(define-key winner-mode-map (kbd "C-c e") #'winner-redo)

;;; =======================================================
;;;
;;;   HDG
;;;
;; HDG's save file is actually JSON for now

(add-to-list 'auto-mode-alist '("\\.hdgsave\\'" . js-json-mode))

;;; =======================================================
;;;
;;;   Lua Mode
;;;

(use-package lua-mode
  :ensure t
  :config
  (setq lua-indent-level 4)
  (setq lua-indent-close-paren-align nil)
  )


;;; =======================================================
;;;
;;;   Rust Mode
;;;

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  )

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(add-hook 'rust-mode-hook 'eglot-ensure)

(use-package cargo-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
		  

;;; ------------ Welcome Message --------------

(message "init.el has been loaded!")

