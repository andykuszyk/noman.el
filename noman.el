;;; noman.el --- Read command line help without a man page -*- lexical-binding:t; ispell-buffer-session-localwords: ("noman" "subcommand" "subcommands"); -*-

;; Copyright (C) 2024 Andy Kuszyk

;; Author: Andy Kuszyk <emacs@akuszyk.com>
;; URL: https://github.com/andykuszyk/noman.el
;; Version: 0.4
;; Keywords: docs
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Noman is a package that parses command line help from flags like
;; --help, and presents it in an easy-to-navigate Emacs buffer.

;;; Code:
(require 'cl-lib)
(require 'ansi-color)

(defgroup noman nil "Command line help reader." :group 'applications :prefix "noman-")

(defcustom noman-parsing-functions
  '(("aws" . noman--make-aws-button))
  "Alist of form: ((COMMAND . PARSER)).
COMMAND is a string matched against the requested command.
PARSER  is a function which accepts a line of text and returns a button or nil."
  :type 'alist)

(defcustom noman-reuse-buffers
  t
  "Controls whether or not a new buffer is created for each noman invocation.
The default value of t displays all command help in the same buffer named
  *noman*.
Any other value results in a new buffer being created for each command, with the
  name *noman <command>*"
  :type 'boolean)

(defcustom noman-shell-file-name
  nil
  "An override for the default `shell-file-name'.
If set, this value will used when displaying help for shell built-in commands."
  :type 'string)

(defvar-local noman--last-command nil "The last command that noman executed.")
(defvar-local noman--buttons nil "A list of buttons in the current noman buffer.")
(defvar noman--history nil "History of recent noman commands.")

(defun noman-menu (subcommand)
  "Choose the SUBCOMMAND to view help for."
  (interactive
   (list
    (completing-read
     "Sub-command: "
     (or (mapcar #'button-label noman--buttons)
         (user-error "No subcommands found")))))
  (when-let ((button
	      (cl-loop
               for b in noman--buttons
               thereis
	       (when
		   (string=
		    subcommand
		    (button-label b))
		 b))))
    (button-activate button)))

(defun noman-back ()
  "Go to the previous subcommand."
  (interactive)
  (if (not (> (length noman--history) 1))
      (user-error "End of noman history")
    (pop noman--history) ;; pop current command
    (let ((previous-cmd (pop noman--history)))
      (message previous-cmd)
      (noman previous-cmd))))

(defun noman--follow-link (button)
  "Follow a link from BUTTON in a noman buffer."
  (let ((subcommand (button-label button)))
    (noman (format "%s %s" noman--last-command subcommand))))

(defun noman--make-aws-button (line)
  "Return button for aws-style command LINE."
  (when-let
      ((first-match
	(string-match "^ +o +\\([A-Za-z0-9\\-]+\\)$" line))
       (beg (match-beginning 1))
       (end (match-end 1))
       (bol (line-beginning-position)))
    (make-button
     (+ bol beg)
     (+ bol end)
     'action
     #'noman--follow-link)))

(defun noman--button (line)
  "Return default command LINE button."
  (when-let
      (((string-prefix-p "  " line))
       (first-match
	(string-match
         "^ +\\([A-Za-z]+[A-Za-z0-9\\-]+\\):* \\{2\\}.*$"
         line))
       (beg (match-beginning 1))
       (end (match-end 1))
       (bol (line-beginning-position)))
    (make-button
     (+ bol beg)
     (+ bol end)
     'action
     #'noman--follow-link)))

(defun noman--button-func (cmd)
  "Gets the function to use for parsing subcommands for the given CMD."
  (alist-get
   (car (split-string cmd " " 'omit-nulls))
   noman-parsing-functions #'noman--button nil #'string=))

(defun noman--buttonize (cmd)
  "Evaluate CMD's button function on each line in `current-buffer'.
Return list of created buttons."
  (let ((button-func (noman--button-func cmd))
        (buttons nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (push (funcall button-func
                       (buffer-substring (line-beginning-position) (line-end-position)))
              buttons)
        (forward-line)))
    (delq nil buttons)))

(defvar-keymap noman-mode-map
  "m" #'noman-menu
  "g" #'noman-menu
  "G" #'noman
  "n" #'next-line
  "p" #'previous-line
  "l" #'noman-back
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(define-derived-mode noman-mode special-mode "noman"
  "Major mode for browsing command line help.

\\{noman-mode-map}")

(defun noman--generate-buffer-name (cmd)
  "Generate a buffer name from CMD.
If noman-reuse-buffers is t, *noman* will always be returned."
  (if noman-reuse-buffers
      "*noman*"
    (format "*noman %s*" cmd)))

(defun noman--buffer (cmd)
  "Prepare and display noman CMD's noman buffer."
  (let* ((tokens (split-string cmd))
         (prefix (car tokens))
         (type (string-trim (shell-command-to-string (concat "command -V " prefix))))
         (inhibit-read-only t))
    (with-current-buffer (get-buffer-create (noman--generate-buffer-name cmd))
      (erase-buffer)
      (cond
       ((string-suffix-p " is a shell builtin" type)
	(message "shell built in")
	(shell-command
	 (format
	  "%s -c 'help %s'"
	  (if noman-shell-file-name noman-shell-file-name shell-file-name)
	  cmd)
	 (current-buffer)
	 nil))
       ((string-suffix-p " not found" type)
        (user-error "Command '%s' not found" prefix))
       (t (unless (= (apply #'call-process
			    prefix
			    nil
			    t
			    nil
			    `(,@(cdr tokens) "--help"))
                     0)
            (erase-buffer)
	    (apply #'call-process
		   prefix
		   nil
		   t
		   nil
		   `(,@(cdr tokens) "help"))
	    (replace-regexp-in-region "." "" (point-min) (point-max)))
          (when-let ((versioninfo
                      (save-excursion
                        (with-temp-buffer
                          (unless (= (call-process prefix nil t nil  "--version") 0)
                            (erase-buffer)
                            (call-process prefix nil t nil "version")
                            (replace-regexp-in-region "." "" (point-min) (point-max)))
                          (indent-code-rigidly (point-min) (point-max) 4)
                          (buffer-string))))
                     (versioninfo-p (not (string-empty-p (string-trim versioninfo)))))
            (goto-char (point-max))
            (insert "\nIMPLEMENTATION\n")
            (insert versioninfo))))
      (ansi-color-apply-on-region (point-min) (point-max))
      (goto-char (point-min))
      (unless (derived-mode-p 'noman-mode) (noman-mode))
      (setq noman--buttons (noman--buttonize cmd)
            noman--last-command cmd
            header-line-format (string-join (split-string cmd " ")  " > "))
      (display-buffer (current-buffer) '(display-buffer-reuse-window)))))

(defun noman--read-cmd ()
  "Return executable command from user prompt."
  (cl-loop for path in (cl-loop for p in (exec-path)
                                append (if (file-directory-p p)
                                           (directory-files p t "^[^.].*")
                                         (list p)))
           when (and  (file-executable-p path))
           collect (file-name-nondirectory path) into commands
           finally return (completing-read "Program: " commands nil t)))

;;;###autoload
(defun noman (cmd)
  "Display command line help for CMD.
If any prefix argument is used, and command (including shell built-ins) can be
provided.  Otherwise, an executable program from PATH will be prompted for."
  (interactive
   (if current-prefix-arg
       (list (read-string "Command: "))
     (list (noman--read-cmd))))
  (noman--buffer cmd)
  (push cmd noman--history))

(provide 'noman)
;;; noman.el ends here
