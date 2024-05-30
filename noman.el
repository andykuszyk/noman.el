;;; noman.el --- Read command line help without a man page -*- lexical-binding:t; ispell-buffer-session-localwords: ("noman" "subcommand" "subcommands"); -*-

;; Copyright (C) 2024 Andy Kuszyk

;; Author: Andy Kuszyk <emacs@akuszyk.com>
;; URL: https://github.com/andykuszyk/noman.el
;; Version: 0.6
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
  '(("aws" . noman--make-aws-button)
    ("npm" . noman--make-npm-button)
    ("go"  . noman--make-go-button))
  "Alist of form: ((COMMAND . PARSER)).
COMMAND is a string matched against the requested command.
PARSER  is a function which accepts a line of text and returns a button or nil."
  :type '(repeat (cons (string :tag "Command")
                       (symbol :tag "Function"))))

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

(defcustom noman-help-format
  '(("npm" (command "help" args))
    ("go" (command "help" args)))
  "Control how help is called for subcommands.
COMMAND is replaced with the main command.
ARGS is replaced with arguments for subcommand help.
Strings are interpreted as is."
  :type '(repeat (list (string :tag "Command")
                       (repeat (choice (const command)
                                       (const args)
                                       string)))))

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

(defun noman--make-aws-button (&rest _)
  "Return button for aws-style command LINE."
  (when (looking-at "^ +o +\\([A-Za-z0-9\\-]+\\)$")
    (list (cons (match-beginning 1) (match-end 1)))))

(defun noman--make-npm-button (&optional subcommand-p)
  "Return button positions for npm subcommands.
SUBCOMMAND-P is non-nil when parsing a subcommand."
  (if subcommand-p
      (when (looking-at "^       •   npm help \\([a-z-]+\\)")
        (list (cons (match-beginning 1) (match-end 1))))
    (if (looking-at-p "^ \\{4\\}[a-z-]+,")
        (let (res)
          (while (re-search-forward "\\([a-z-]+\\),?" (line-end-position) t)
            (push (cons (match-beginning 1) (match-end 1)) res))
          res))))

(defun noman--make-go-button (&optional subcommand-p)
  "Return button positions for go subcommands.
SUBCOMMAND-P is non-nil when parsing a subcommand."
  (if subcommand-p
      (when (looking-at-p "^See also: ")
        (let (res)
          (while (re-search-forward "go \\([a-z.]+\\)" (line-end-position) t)
            (push (cons (match-beginning 1) (match-end 1)) res))
          res))
    (if (looking-at "^\t\\([a-z.-]+\\)")
        (list (cons (match-beginning 1) (match-end 1))))))

(defun noman--button (&rest _)
  "Return default command LINE button."
  (when (looking-at "^  \\([A-Za-z]+[A-Za-z0-9\\-]+\\):* \\{2\\}.*$")
    (list (cons (match-beginning 1) (match-end 1)))))

(defun noman--button-func (cmd)
  "Gets the function to use for parsing subcommands for the given CMD."
  (alist-get
   (car (split-string cmd " " 'omit-nulls))
   noman-parsing-functions #'noman--button nil #'string=))

(defun noman--buttonize (cmd)
  "Evaluate CMD's button function on each line in `current-buffer'.
Return list of created buttons."
  (let ((button-func (noman--button-func cmd))
        (subcommand-p (string-search " " cmd))
        (buttons nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((btns (funcall button-func subcommand-p)))
          (or (listp btns) (setq btns (list btns)))
          (dolist (pos btns)
            (push (if (overlayp pos) pos
                    (make-button (car pos) (cdr pos)
                                 'action #'noman--follow-link))
                  buttons)))
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
If `noman-reuse-buffers' is t, *noman* will always be returned."
  (if noman-reuse-buffers
      "*noman*"
    (format "*noman %s*" cmd)))

(defun noman--build-help-command (cmd &optional args)
  "Build help command for CMD with ARGS."
  (if-let ((order (car (assoc-default cmd noman-help-format))))
      (delq nil (mapcan (lambda (arg)
                          (pcase arg
                            ((pred stringp) (list arg))
                            ('command (list cmd))
                            ('args args)
                            (_ (error "Unmatched help arg: '%S'" arg))))
                        order))
    (cons cmd (append args '("--help")))))

(defun noman--build-alt-help-command (args)
  "Build alternative help command from ARGS.
Swaps \"help\" for \"--help\" and vice versa."
  (mapcar (lambda (e)
            (pcase e
              ("help" "--help")
              ("--help" "help")
              (_ e)))
          args))

(defun noman--call-help-commands (args)
  "Call help command ARGS and its alternative if it fails."
  (let ((cmd (car args))
        (args (cdr args)))
    (unless (zerop (apply #'call-process cmd nil t nil args))
      (erase-buffer)
      (apply #'call-process cmd nil t nil (noman--build-alt-help-command args)))))

(defun noman--buffer (cmd)
  "Prepare and display noman CMD's noman buffer."
  (let* ((tokens (split-string cmd))
         (prefix (car tokens))
         (type (string-trim (shell-command-to-string
			     (concat
			      "command -V "
			      (shell-quote-argument prefix)))))
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
	  (shell-quote-argument cmd))
	 (current-buffer)
	 nil))
       ((string-suffix-p " not found" type)
        (user-error "Command '%s' not found" prefix))
       (t (let ((args (noman--build-help-command prefix (cdr tokens))))
            (noman--call-help-commands args)
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
