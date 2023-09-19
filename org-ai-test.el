(defcustom org-ai-hl-highlighted-words '("\[SYS\]:" "\[ME\]:" "\[AI\]:")
  "Words to highlight"
  :group 'org-ai-hl-mode)

(defvar org-ai-hl-search-list-re (regexp-opt org-ai-hl-highlighted-words)
  "regexp constructed from 'org-ai-hl-highlighted-words")

(defface font-lock-org-ai-hl-face '((((background light)) (:background "#8200f9"))
                                    (((background dark)) (:background "blue")))
  "Face used for highlighting AI keywords."
  :group 'org-ai-hl-mode)

(defun org-ai-hl-fontify-keyword (limit)
  "Highlight the next org-ai keyword in the buffer, up to LIMIT."
  (save-match-data
    (while (re-search-forward org-ai-hl-search-list-re limit t)
      (add-text-properties
       (match-beginning 0) (match-end 0)
       '(face font-lock-org-ai-hl-face)))))

(defun org-ai-hl-fontify-ai-block (limit)
  "Function used to advice `org-fontify-meta-lines-and-blocks-1' to
highlight keywords in AI blocks."
  (let ((case-fold-search t))
    (save-match-data
      (when (re-search-forward "^#\\+begin_ai[^\t\n]*" limit t)
        (let ((beg (match-beginning 0))
	      (end-of-beginline (match-end 0))
	      ;; Including \n at end of #+begin line will include \n
	      ;; after the end of block content.
	      (block-start (match-end 0))
	      (block-end nil)
	      (bol-after-beginline (line-beginning-position 2))
              (whole-blockline org-fontify-whole-block-delimiter-line))

          (when (re-search-forward "#\\+end_ai" nil t)
	    (setq block-end (match-beginning 0)
                  beg-of-endline (match-beginning 0)
		  end-of-endline (match-end 0))

            ;; Fontify the block content
            (add-text-properties
	     beg end-of-endline '(font-lock-fontified t font-lock-multiline t))

            (add-text-properties
	     block-start beg-of-endline '(face org-block))

            ;; Fontify markdown code blocks
            (save-excursion
              (goto-char block-start)
              (while (re-search-forward "^```\\(.*\\)" block-end t)
                (let ((beg-1 (match-beginning 0))
                      (md-start (+ 1 (match-end 0)))
                      (lang (match-string 1)))
                  ;; if we are inside an org block, don't fontify so that the
                  ;; syntax highlighting is less confusing
                  (when (or (string= lang "org")
                            (string= lang "org-mode"))
                    (setq lang nil))
                  (when (re-search-forward "^```$" block-end t)
                    (let ((md-end (match-beginning 0))
                          (end-2 (match-end 0)))
                      ;; prefer markdown  mode for highlighting if  available as
                      ;; it does a better job, but fall back to org mode
                      (if (fboundp 'markdown-fontify-code-block-natively)
                          (markdown-fontify-code-block-natively lang md-start md-end)
                        (org-src-font-lock-fontify-block lang md-start md-end)))))))

            ;; Fontify `org-ai-hl-highlighted-words' in the block
            (save-excursion
              (goto-char block-start)
              (org-ai-hl-fontify-keyword block-end))

	    ;; Fontify the #+begin and #+end lines of the blocks
	    (add-text-properties
	     beg (if whole-blockline bol-after-beginline end-of-beginline)
	     '(face org-block-begin-line))
            (unless (eq (char-after beg-of-endline) ?*)
	      (add-text-properties
	       beg-of-endline
	       (if whole-blockline
	           (let ((beg-of-next-line (1+ end-of-endline)))
	             (min (point-max) beg-of-next-line))
	         (min (point-max) end-of-endline))
	       '(face org-block-end-line)))))))))


(advice-add 'org-fontify-meta-lines-and-blocks-1 :before #'org-ai-hl-fontify-ai-block)
