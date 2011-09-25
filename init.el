(setq inhibit-startup-message t)

;; 2010-04-21, for emacsclient
(server-start)

(prefer-coding-system 'utf-8)

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
(add-to-list 'load-path "/usr/share/emacs/site-lisp" t)

;; Go (golang.org), 2011-05-26
(add-to-list 'load-path "~/software/go/misc/emacs" t)
(require 'go-mode-load)

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
      (require 'itunes)))

;; Nice anti-aliased fonts on Linux. Works out of the box since 23.1.
;; Also needs "Emacs.FontBackend: xft" in ~/.Xresources.
;; <http://www.emacswiki.org/emacs/XftGnuEmacs>
(if (string-match "linux" system-configuration)
    (progn
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "firefox")
      (custom-set-faces '(default ((t (:height 124 :family "Inconsolata")))))))


(defun s5-build-slides ()
  "Build slides from a Markdown document using my Perl script."
  (interactive)

  (defun insert-timestamp (buffer)
    "Insert 'current-time-string' at the end of BUFFER."
    (save-excursion (set-buffer buffer)
                    (goto-char (point-max))
                    (insert "\n" (current-time-string) "\n")))

  (defun build-slides-from-buffer (output-buffer)
    "Build S5 slides depending on the file-local S5 style.
If there is a file-local style, the build script needs it as an
argument, but if there's none, we must omit it. A nil argument
won't do, as call-process chokes on it.
Return the exit code of the external build script."
    (let* ((script "~/Dropbox/perl/Thomas/S5/Builder.pm")
           (build-function
            (apply-partially 'call-process script nil output-buffer nil
                             (buffer-file-name))))
      (if (boundp 's5-style)
          (funcall build-function s5-style)
        (funcall build-function))))

  (let* ((output-buffer-name "*s5*")
         (output-buffer (get-buffer-create output-buffer-name)))
    (insert-timestamp output-buffer)
    (if (> (build-slides-from-buffer output-buffer) 0)
        (display-buffer output-buffer)
      (message "Slides built, more info in %s." output-buffer-name))))


(defun my-big-screen ()
  "Set up frame for external screen, with three windows."
  (interactive)
  (my-initialize-frame 3))

(defun my-small-screen ()
  "Set up frame for laptop screen, with two windows."
  (interactive)
  (my-initialize-frame 2))

(defun my-initialize-frame (columns)
  "Set current frame to fullscreen and split it into COLUMNS
vertical windows."
  (set-frame-parameter nil :fullscreen t)
  (delete-other-windows)
  (dotimes (not-used (1- columns)) 
    (split-window-horizontally))
  (balance-windows))
  

(setq frame-title-format
      (list (format "Emacs: %%j")
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


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
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq kill-whole-line t) ; C-k deletes the end of line
(setq woman-use-own-frame nil)
(setq-default indent-tabs-mode nil) ; no tabs
(ffap-bindings) ; find-file-at-point
(blink-cursor-mode 0)
(setq x-select-enable-clipboard t)


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

(defun my-perltidy-buffer ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "perltidy" nil t))


;;; Stefan Monnier <foo at acm.org>. It is the opposite of
;;; fill-paragraph.
(defun my-unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Comment and uncomment lines. From
;; http://www.emacswiki.org/emacs/CommentingCode.
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun my-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at
the end of the line, then comment current line. Replaces default behaviour
of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (progn
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))
        (forward-line 1))
    (comment-dwim arg)))

(global-set-key "\M-7" 'my-comment-dwim-line)


;; 2010-09-10
;; http://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
(defun my-show-buffer-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name) ))


;; 2010-03-11
;; http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
(defun my-buffer-file-name-on-clipboard (&optional file-only-p)
  "Put the current file name on the clipboard"
  (interactive "P")
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (if file-only-p
                        (file-name-nondirectory (buffer-file-name))
                      (buffer-file-name)))))
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

(load-file "/usr/share/emacs/site-lisp/erlang/erlang.el")
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


;; 2011-08-07 https://github.com/rooney/zencoding
(require 'zencoding-mode)


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


;; OCaml
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)


;;; PERL ;;;

(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.t$"  . perl-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . perl-mode))

(defun thomas11-perl-indent-setup ()
  (setq cperl-indent-level 4
        cperl-close-paren-offset -2
        cperl-indent-parens-as-block t))
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


;; Zeitgeist from
;; https://code.launchpad.net/~patrickniedzielski/zeitgeist-dataproviders/emacs-zeitgeist
(load-library "zeitgeist.elc")


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "pdflatex")
 '(TeX-PDF-mode t)
 '(delete-selection-mode nil)
 '(mark-even-if-inactive t)
 '(transient-mark-mode 1))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(writegood-passive-voice-face ((((class color)) (:background "moccasin"))))
 '(writegood-weasels-face ((((class color) (background light)) (:background "moccasin")))))
