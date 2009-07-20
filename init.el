(setq inhibit-startup-message t)


(setq user-mail-address "tkappler@gmail.com")


;; Set load-path, including subdirectories.
;; 2009-04-18 from http://www.emacswiki.org/emacs/LoadPath
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/elisp/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

(setq Info-additional-directory-list 
      (quote ("/sw/share/info" "/Users/thomas/.emacs.d/usr/share/info")))


;; Save backups in one place instead of clobbering the disk.
;; 2009-04-20 from http://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t                                   ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.emacs.d/saves"))  ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)                                    ; use versioned backups


(global-font-lock-mode t)
(show-paren-mode t)
(column-number-mode t)
(setq visible-bell t)
(scroll-bar-mode nil)
(setq kill-whole-line t) ; C-k deletes the end of line


;; "fix" the broken keyboard. 
;; 2009-03-17 from http://www.emacswiki.org/emacs/EmacsOnGermanMac
(global-set-key "\M-l" '(lambda () (interactive) (insert "@")))
(global-set-key "\M-5" '(lambda () (interactive) (insert "[")))
(global-set-key "\M-6" '(lambda () (interactive) (insert "]")))
(global-set-key "\M-7" '(lambda () (interactive) (insert "|")))
(global-set-key "\M-/" '(lambda () (interactive) (insert "\\")))
(global-set-key "\M-8" '(lambda () (interactive) (insert "{")))
(global-set-key "\M-9" '(lambda () (interactive) (insert "}")))
(global-set-key "\M-n" '(lambda () (interactive) (insert "~")))


;; 2009-07-19 see http://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
(ido-mode t)


;; Indent pasted and yanked lines automatically. From
;; http://www.emacswiki.org/emacs/AutoIndentation.
(defadvice yank (after indent-region activate)
  (if (member major-mode '(cperl-mode perl-mode emacs-lisp-mode scheme-mode lisp-mode
				      c-mode c++-mode objc-mode
				      latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
	(indent-region (region-beginning) (region-end) nil))))
(defadvice yank-pop (after indent-region activate)
  (if (member major-mode '(cperl-mode perl-mode emacs-lisp-mode scheme-mode lisp-mode
				      c-mode c++-mode objc-mode
				      latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
	(indent-region (region-beginning) (region-end) nil))))


;; Move between windows using shift-arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; 2009-07-20 from http://www.adamspiers.org/elisp/smooth-scrolling.el
(require 'smooth-scrolling)


;; MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2009-07-20 iTunes control from http://www.emacswiki.org/emacs/itunes.el
(require 'itunes)


;; JavaScript mode by Steve Yegge
;; 2009-04-18 from http://code.google.com/p/js2-mode/
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; JavaScript repl inside emacs: http://js-comint-el.sourceforge.net/
(require 'js-comint)
(setq inferior-js-program-command "js")
(add-hook 'js2-mode-hook '(lambda ()
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)))


(autoload 'io-mode "io-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.io$" . io-mode))


;; 2009-04-07 from http://www.emacswiki.org/emacs/HtmlEndOfLine
(defun html-end-of-line ()
  "If there is an HTML tag at the end of the line, then go to start of tag.
  Otherwise go to the real end of the line."
  (interactive)
  (if (or (looking-at ".*>$") ; if we're on a line that ends with a tag
          (and (= (char-before) 62)
               (= (point) (save-excursion
                            (end-of-line)
                            (point))))) ; or we're at the end of a line with a tag
      (let ((where-now (point)))
        (narrow-to-region
         (save-excursion
           (beginning-of-line)
           (point))
         (save-excursion
           (end-of-line)
           (point)))
        (end-of-line)
        (re-search-backward "<" nil t)
        (if (= (point) where-now)
            (end-of-line))
        (widen))
    (end-of-line)))
(add-hook 'nxml-mode-hook
          (lambda ()
            (define-key html-helper-mode-map "\C-e" 'html-end-of-line)))


;; Muse mode, http://mwolson.org, added 2009-05-03
(require 'muse-mode)
(require 'muse-publish)


;; Markdown mode from http://jblevins.org/projects/markdown-mode/ for 
;; http://daringfireball.net/projects/markdown/, 2009-06-20
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(setq markdown-command "Markdown.pl")


;; Easy access to PerlTidy, from
;; http://www.emacswiki.org/emacs/CPerlMode (comments).
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
		  (perltidy-region)))
