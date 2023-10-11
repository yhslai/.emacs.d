
;; Auto-generated. Don't touch these.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-visited-interval 3)
 '(auto-save-visited-mode t)
 '(mode-line-compact 'long)
 '(org-agenda-files '("~/.emacs.d/org-test/1.org"))
 '(package-selected-packages
   '(eglot markdown-mode cargo-mode rust-mode projectile-ripgrep org-ai luarocks evil-owl zoom string-inflection corfu orderless vertico projectile yaml-mode tree-sitter lua-mode languagetool helpful undo-tree visual-fill-column evil-collection magit which-key evil))
 '(package-vc-selected-packages
   '((lua-ts-mode :vc-backend Git :url "https://git.sr.ht/~johnmuhl/lua-ts-mode")))
 '(safe-local-variable-values
   '((eval message "Cargo.toml file is `%s'."
		   (concat hdgtools-path "/Cargo.toml"))
	 (eval set
		   (make-local-variable 'hdgtools-path)
		   (concat my-project-path "Horny Dungeon Project/PipelineTools/hdgtools"))
	 (eval message "hdgtools directory set to `%s'." hdgtools-path)
	 (eval set
		   (make-local-variable 'hdgtools-path
								(concat my-project-path "Horny Dungeon Project/PipelineTools/hdgtools")))
	 (eval message "Project directory set to `%s'." my-project-path)
	 (eval set
		   (make-local-variable 'my-project-path)
		   (file-name-directory
			(let
				((d
				  (dir-locals-find-file ".")))
			  (if
				  (stringp d)
				  d
				(car d)))))))
 '(send-mail-function 'mailclient-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
