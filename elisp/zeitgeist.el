;;; The Zeitgeist Emacs Script -- integrates Emacs with Zeitgeist.
;;; Copyright (C) 2010, Patrick M. Niedzielski <PatrickNiedzielski@gmail.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; For more information about the Zeitgeist Project, see
;;; <http://zeitgeist-project.com/>.

(require 'dbus)


(defun zeitgeist-call (method &rest args)
  "Call the zeitgeist method METHOD with ARGS over dbus"
  (apply 'dbus-call-method 
    :session                            ; use the session (not system) bus
    "org.gnome.zeitgeist.Engine"        ; service name
    "/org/gnome/zeitgeist/log/activity" ; path name
    "org.gnome.zeitgeist.Log"           ; interface name
    method args))

(defun zeitgeist-event-timestamp ()
  "Get the timestamp in zeitgeist format."
  (let* ((now-time (current-time))
         (hi       (car now-time))
         (lo       (car (cdr now-time)))
         (msecs    (car (cdr (cdr now-time))))) ; This is *micro*seconds. 

  (substring (number-to-string (+ (/ msecs 1000)
             (* (+ lo (* hi 65536.0))     1000))) 0 -2))) ; Convert system time to milliseconds.

(defun zeitgeist-get-mime-type ()
  "Get the mime type from the extension."
  (let ((ext (file-name-extension (buffer-file-name))))
   ; Maybe use file mode later...
   (cond ((string= "el" ext)      "text/x-script.elisp")

         ((string= "cpp" ext)     "text/x-c++src")
         ((string= "C" ext)       "text/x-c++src")
         ((string= "c++" ext)     "text/x-c++src")
         ((string= "cxx" ext)     "text/x-c++src")
         ((string= "cc" ext)      "text/x-c++src")

         ((string= "hpp" ext)     "text/x-c++hdr")
         ((string= "h++" ext)     "text/x-c++hdr")
         ((string= "hxx" ext)     "text/x-c++hdr")
         ((string= "hh" ext)      "text/x-c++hdr")

         ((string= "csv" ext)     "text/comma-separated-values")

         ((string= "h" ext)       "text/x-chdr")

         ((string= "c" ext)       "text/x-csrc")

         ((string= "java" ext)    "text/x-java")

         ((string= "p" ext)       "text/x-pascal")
         ((string= "pas" ext)     "text/x-pascal")

         ((string= "tcl" ext)     "text/x-tcl")
         ((string= "tk" ext)      "text/x-tcl")

         ((string= "tex" ext)     "text/x-tex")
         ((string= "sty" ext)     "text/x-tex")
         ((string= "cls" ext)     "text/x-tex")

         ((string= "html" ext)    "text/html")
         ((string= "htm" ext)     "text/html")

         ((string= "latex" ext)   "application/x-latex")
         ((string= "ltx" ext)     "application/x-latex")

         ((string= "sh" ext)      "application/x-sh")

         ((string= "pl" ext)      "application/x-perl")
         ((string= "pm" ext)      "application/x-perl")

         ((string= "texinfo" ext) "application/x-texinfo")
         ((string= "texi" ext)    "application/x-texinfo")

         ((string= "t" ext)       "application/x-troff")
         ((string= "tr" ext)      "application/x-troff")
         ((string= "roff" ext)    "application/x-troff")

         ((string= "xml" ext)     "text/xml")
         ((string= "xsd" ext)     "text/xml")

         ((string= "xslt" ext)    "application/xslt+xml")
         ((string= "xsl"  ext)    "application/xslt+xml")

         ((string= "txt" ext)     "text/plain")
         (t                       "text/plain"))))

(defun zeitgeist-event-interpretation (event)
  "Get the Event Interpretation of EVENT."
  (cond
    ((eq event 'zeitgeist-open-event)
       "http://www.zeitgeist-project.com/ontologies/2010/01/27/zg#AccessEvent")
    ((eq event 'zeitgeist-close-event)
       "http://www.zeitgeist-project.com/ontologies/2010/01/27/zg#LeaveEvent")
    ((eq event 'zeitgeist-create-event)
       "http://www.zeitgeist-project.com/ontologies/2010/01/27/zg#CreateEvent")
    ((eq event 'zeitgeist-modify-event)
       "http://www.zeitgeist-project.com/ontologies/2010/01/27/zg#ModifyEvent")
    (t nil)))

(defun zeitgeist-get-file-interpretation ()
  "Get the File Interpretation based on the mime type."
  (let ((mime (zeitgeist-get-mime-type)))
   ; Maybe use file mode later...
   (cond ((string= "text/x-script.elisp" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "text/x-c++src" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "text/x-c++hdr" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "text/comma-separated-values" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#PlainTextDocument")

         ((string= "text/x-chdr" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "text/x-csrc" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "text/x-java" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "text/x-pascal" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "text/x-tcl" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "text/x-tex" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "text/html" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#HtmlDocument")

         ((string= "application/x-latex" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "application/x-sh" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "application/x-perl" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "application/x-texinfo" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "application/x-troff" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "text/xml" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "application/xslt+xml" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#SourceCode")

         ((string= "text/plain" mime)
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#PlainTextDocument")
         (t
         "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#PlainTextDocument"))))

(defun zeitgeist-send (event fileurl filemime)
  "Send zeitgeist an event EVENT using the list FILEINFO."
  (let ((event-interpretation (zeitgeist-event-interpretation event)))
    (if (eq nil event-interpretation)
      (message "YOU FAIL")
      (zeitgeist-call "InsertEvents"
        (list (list :struct (list ""
              (zeitgeist-event-timestamp)
              event-interpretation
              "http://www.zeitgeist-project.com/ontologies/2010/01/27/zg#UserActivity"
              "application://emacs23.desktop")
        (list (list (concat "file://" fileurl)
           (zeitgeist-get-file-interpretation)
           "http://www.semanticdesktop.org/ontologies/nfo#FileDataObject"
           (concat "file://" (file-name-directory fileurl))
           filemime
           (file-name-nondirectory (file-name-sans-versions fileurl))
           "")) ; Some black magic later?
        '(:array :byte 0)))))))

(defun zeitgeist-open-file ()
  "Tell zeitgeist we opened a file!"
  (if (eq nil (buffer-file-name))
      (message "You are not on a file.")
    (zeitgeist-send 'zeitgeist-open-event
                    buffer-file-name
                    (zeitgeist-get-mime-type))))
(defun zeitgeist-close-file ()
  "Tell zeitgeist we closed a file!"
  (if (eq nil (buffer-file-name))
      (message "You are not on a file.")
    (zeitgeist-send 'zeitgeist-close-event
                    buffer-file-name
                    (zeitgeist-get-mime-type))))
(defun zeitgeist-create-file ()
  "Tell zeitgeist we created a file!"
    (zeitgeist-send 'zeitgeist-create-event
                    buffer-file-name
                    (zeitgeist-get-mime-type)))
(defun zeitgeist-modify-file ()
  "Tell zeitgeist we modified a file!"
    (zeitgeist-send 'zeitgeist-modify-event
                     buffer-file-name
                     (zeitgeist-get-mime-type)))

(defun zeitgeist-find-file-hook ()
  "Call zeitgeist-open-file if the file exists."
  (if (file-exists-p (buffer-file-name))
      (zeitgeist-open-file)))
(defun zeitgeist-kill-buffer-hook ()
  "Call zeitgeist-close-file if the file exists."
  (if (and (not (eq nil (buffer-file-name)))
            (file-exists-p (buffer-file-name)))
      (zeitgeist-close-file)))
(defun zeitgeist-kill-emacs-hook ()
  "Call zeitgeist-close-file on all files that exist."
  (mapc '(lambda (buffer)
	   (set-buffer buffer)
	   (zeitgeist-close-file))
	  (buffer-list)))
(defun zeitgeist-before-save-hook ()
  "Call zeitgeist-modify-file or zeitgeist-create-file."
  (if (and (not (eq nil (buffer-file-name)))
            (file-exists-p (buffer-file-name)))
      (zeitgeist-modify-file)
      (zeitgeist-create-file)))

(add-hook 'find-file-hook   'zeitgeist-find-file-hook)
(add-hook 'kill-buffer-hook 'zeitgeist-kill-buffer-hook)
(add-hook 'kill-emacs-hook  'zeitgeist-kill-emacs-hook)
(add-hook 'before-save-hook 'zeitgeist-before-save-hook)

;;; zeitgeist.el ends here
