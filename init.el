(setq inhibit-startup-message t)

;; 2010-04-21, for emacsclient
(server-start)

(setq user-mail-address "tkappler@gmail.com")
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "tkappler@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "thomas.foo")


;; Set load-path, including subdirectories.
;; 2009-04-18 from http://www.emacswiki.org/emacs/LoadPath
(when (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (setq load-path (cons "~/.emacs.d/" load-path))
    (progn (cd "~/.emacs.d/")
           (normal-top-level-add-subdirs-to-load-path)
           (cd "~/Dropbox")))

(setq Info-additional-directory-list '("~/.emacs.d/usr/share/info"
                                       "~/software/usr/share/info"))


;; 2009-08-02: If we're on Mac OS, set the path, as Mac OS doesn't
;; pass it on to apps launched from the finder or the dock by default;
;; see <http://www.emacswiki.org/emacs/MacOSTweaks>.
;; Also add fink's info path.
(if (string-match "darwin" system-configuration)
    (progn
      (setenv "PATH"
              (concat (getenv "PATH")   ":/usr/local/bin"))
      (setq exec-path 
            (append exec-path '("/usr/local/bin")))
      (add-to-list Info-additional-directory-list '("/sw/share/info"))

      ;; 2009-07-20 iTunes control from http://www.emacswiki.org/emacs/itunes.el
      (require 'itunes)

      ;; "fix" the broken keyboard. 
      ;; 2009-03-17 from http://www.emacswiki.org/emacs/EmacsOnGermanMac
      (global-set-key "\M-l" '(lambda () (interactive) (insert "@")))
      (global-set-key "\M-5" '(lambda () (interactive) (insert "[")))
      (global-set-key "\M-6" '(lambda () (interactive) (insert "]")))
      (global-set-key "\M-7" '(lambda () (interactive) (insert "|")))
      (global-set-key "\M-/" '(lambda () (interactive) (insert "\\")))
      (global-set-key "\M-8" '(lambda () (interactive) (insert "{")))
      (global-set-key "\M-9" '(lambda () (interactive) (insert "}")))
      (global-set-key "\M-n" '(lambda () (interactive) (insert "~")))))

;; Nice anti-aliased fonts on Linux. Works out of the box since 23.1.
;; Also needs "Emacs.FontBackend: xft" in ~/.Xresources.
;; <http://www.emacswiki.org/emacs/XftGnuEmacs>
(if (string-match "linux" system-configuration)
    (progn
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "chromium")
      (custom-set-faces '(default ((t (:height 101 :family "Inconsolata")))))))


(defun my-big-screen ()
  (interactive)
  (my-initialize-frame 3))

(defun my-small-screen ()
  (interactive)
  (my-initialize-frame 2))

(defun my-initialize-frame (columns)
  (set-frame-parameter nil :fullscreen t)
  (delete-other-windows)
  (dotimes (not-used (1- columns)) 
    (split-window-horizontally))
  (balance-windows))
  

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
(tool-bar-mode nil)
(menu-bar-mode nil)
(setq kill-whole-line t) ; C-k deletes the end of line
(setq woman-use-own-frame nil)
(setq-default indent-tabs-mode nil) ; no tabs
(ffap-bindings) ; find-file-at-point
(blink-cursor-mode nil)

;; 2009-11-14 md-readme. List emacs projects on github so they are set
;; up for auto-generating their README.md.
(require 'md-readme)
(dir-locals-set-class-variables
 'generate-README-with-md-readme
 '((emacs-lisp-mode . ((mdr-generate-readme . t)))))
(dolist (dir '("~/Dropbox/delim-kill/"
               "~/Dropbox/wpmail/"
               "~/Dropbox/md-readme/"
               "~/Dropbox/simple-journal/"))
  (dir-locals-set-directory-class
   dir 'generate-README-with-md-readme))
(add-hook 'after-save-hook 
          '(lambda () (when (boundp 'mdr-generate-readme) (mdr-generate))))

;; Emacs Lisp mode hooks
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             ;; Make el-expectations available for unit tests, from
             ;; <http://www.emacswiki.org/emacs/EmacsLispExpectations>.
             (require 'el-expectations)
             ;; 2009-11-16 Require the ELisp maintainer helpers when
             ;; editing Emacs Lisp, they can be used to check the
             ;; headers. Thanks Jonas Bernoulli.
             (require 'lisp-mnt)
             (defun my-verify ()
               (interactive)
               (lm-verify nil nil nil t))))


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


;; 2009-07-25
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


;; HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stefan Monnier <foo at acm.org>. It is the opposite of
;;; fill-paragraph.
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


;; 2010-03-11
;; http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (let ((x-select-enable-clipboard t)) (kill-new filename))
      (message filename))))


;; 2010-03-13
;; http://stackoverflow.com/questions/145291/smart-home-in-emacs
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of
line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key (kbd "C-a") 'smart-beginning-of-line)



;; MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2010-05-02 <http://notmuchmail.org/>
(require 'notmuch)

;; 2009-11-07 <>
(require 'now-focus)

;; 2009-10-24 <http://mumble.net/~campbell/emacs/paredit.el>
;;; Disabled 2009-11-14, preventing me from editing (e.g., deleting
;;; parens) is annoying.
;; (autoload 'paredit-mode "paredit"
;;   "Minor mode for pseudo-structurally editing Lisp code."
;;   t)
;; (defun lisp-enable-paredit-hook ())
;; (add-hook 'clojure-mode-hook (lambda() (paredit-mode +1)))
;; (add-hook 'emacs-lisp-mode-hook (lambda() (paredit-mode +1)))
;; (add-hook 'scheme-mode-hook (lambda() (paredit-mode +1)))

; Load Clojure support with SLIME, my way.
(progn
  (require 'slime-autoloads)
  (setq swank-clojure-jar-path
        (list
         "~/software/clojure/clojure_1.0.0/clojure-1.0.0.jar"
         "~/software/clojure/clojure-contrib/clojure-contrib.jar"
         "~/software/clojure/jline.jar"))
  ;; (setq swank-clojure-jar-path
  ;;       "~/software/clojure/clojure_1.0.0/clojure-1.0.0.jar")
  (require 'swank-clojure-autoload)
  (eval-after-load 'slime
    '(progn (require 'swank-clojure)
            (setq slime-lisp-implementations
                  (cons `(clojure ,(swank-clojure-cmd) :init
                                  swank-clojure-init)
                        (remove-if #'(lambda (x) (eq (car x) 'clojure))
                                   slime-lisp-implementations))))))


;; My own stuff
(require 'simple-journal)
(require 'wpmail)


;;; ERLANG ;;;

(setq load-path (cons "/usr/lib/erlang/lib/tools-2.6.5.1/emacs" load-path))
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ess")

(require 'erlang-start)
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

(require 'distel)
(distel-setup)


;;; JAVASCRIPT ;;;

;; JavaScript mode by Steve Yegge
;; 2009-04-18 from http://code.google.com/p/js2-mode/
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; JavaScript repl inside emacs: http://js-comint-el.sourceforge.net/
(require 'js-comint)
(setq inferior-js-program-command "js")
(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x"  'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb"    'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl"    'js-load-file-and-go)))

;; 2009-11-15 from <http://www.corybennett.org/projects/>: make M-x
;; compile run jslint and step through the problems.
(add-hook 'js2-mode-hook
          (lambda ()
            ; make emacs recognize the error format produced by jslint
            (set (make-local-variable 'compilation-error-regexp-alist)
                 '(("^\\([a-zA-Z.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))
            (set (make-local-variable 'compile-command)
                 (let ((file (file-name-nondirectory buffer-file-name)))
                   (concat "jslint " file)))))

;;; END JAVASCRIPT ;;


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


;; 2010-09-10 writegood-mode <http://github.com/bnbeckwith/writegood-mode>
(require 'writegood-mode)


;; Markdown mode from http://jblevins.org/projects/markdown-mode/ for 
;; http://daringfireball.net/projects/markdown/, 2009-06-20
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(dolist (suffix '("\\.md$" "\\.markdown$" "\\.mdr$" "\\.text$"))
  (add-to-list 'auto-mode-alist `(,suffix . markdown-mode)))
(defun markdown-custom ()
  "markdown-mode-hook"
  (setq markdown-command "Markdown.pl | SmartyPants.pl -2")
  (auto-fill-mode)
  (writegood-mode))
(add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))


;;; PERL ;;;

(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.t$"  . perl-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . perl-mode))

(defun thomas11-perl-indent-setup ()
  (setq cperl-indent-level 4))
(add-hook 'cperl-mode-hook 'thomas11-perl-indent-setup)

;; Devel::PerlySense, 2010-07-23
(require 'flymake)
(load "perlysense.el")

;; Easy access to PerlTidy, from
;; http://www.emacswiki.org/emacs/CPerlMode (comments).
;; (defun perltidy-region ()
;;   "Run perltidy on the current region."
;;   (interactive)
;;   (save-excursion
;;     (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
;; (defun perltidy-defun ()
;;   "Run perltidy on the current defun."
;;   (interactive)
;;   (save-excursion (mark-defun)
;;                (perltidy-region)))


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "pdflatex")
 '(TeX-PDF-mode t)
 '(delete-selection-mode nil)
 '(mark-even-if-inactive t)
 '(scroll-bar-mode (quote right))
 '(transient-mark-mode 1))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:height 101 :family "Inconsolata"))))
 '(writegood-passive-voice-face ((((class color)) (:background "moccasin"))))
 '(writegood-weasels-face ((((class color) (background light)) (:background "moccasin")))))
