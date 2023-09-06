
;; Unset C-z so we don't accidentally hide Emacs
(global-unset-key (kbd "C-z"))

;; Start with maximized window by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Always show keystrokes in echo area immediately
(setq echo-keystrokes 0.01)

;; Allow installing packages from MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; Set up a custom-file so customization variables (e.g. package-selected-packaged used by Package.el)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))



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
;; (setq-default tab-width 4)

;; misc. ui tweaks
(blink-cursor-mode -1)                                ; steady cursor
(pixel-scroll-precision-mode)                         ; smooth scrolling

;; use common keystrokes by default
(cua-mode)

;; display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width 3)           ; set a minimum width

;; nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

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
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)


;;; =======================================================
;;;
;;;   My own theme
;;;

(set-face-attribute 'default nil :font "fira code-12")
(set-face-attribute 'mode-line nil :font "fira code-10")
(set-face-attribute 'mode-line-inactive nil :font "fira code-10")
(setq-default line-spacing 3)


(global-display-fill-column-indicator-mode 1)
(setq-default fill-column 120)
(setq-default display-fill-column-indicator t)
(setq-default display-fill-column-indicator-character ?|)

;;; ======================================================
;;;
;;;   Magit
;;;

(setq magit-define-global-key-bindings 'recommended)

(message "init.el has been loaded!")

;;; ======================================================
;;;
;;;   Org Mode(s)
;;;

(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets


;;; ======================================================
;;;
;;;   Secret Keys
;;;

(load (concat (file-name-directory user-init-file)
	      "secrets.el"))


;;; ======================================================
;;;
;;;   Custom functions and minor modes
;;;

(recentf-mode 1)

(defun open-all-recent-files ()
    "Open all recent files."
    (interactive)
    (dolist (file recentf-list) (find-file file)))
