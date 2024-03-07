;;; noman.el --- Read command line help without a man page -*- lexical-binding:t; ispell-buffer-session-localwords: ("noman" "subcommand" "subcommands"); -*-

;; Copyright (C) 2024 Andy Kuszyk

;; Author: Andy Kuszyk <emacs@akuszyk.com>
;; URL: https://github.com/andykuszyk/noman.el
;; Version: 0.1
;; Keywords: docs
;; Package-Requires: ((emacs "28.1"))

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

(defvar noman--last-command nil
  "The last command that noman executed.")

(defvar-local noman--buttons '()
  "A list of buttons in the current noman buffer.")

(defvar noman--history '()
  "The history of recent noman commands.")

(defun noman-menu (subcommand)
  "Choose the SUBCOMMAND to view help for."
  (interactive (list
		(completing-read
		 "Sub-command: "
		 (cl-mapcar #'button-label noman--buttons))))
  (catch 'noman--button-found
    (dolist (button noman--buttons)
      (when (string=
	     subcommand
	     (button-label button))
	(button-activate button)
	(throw 'noman--button-found t)))))

(defun noman-back ()
  "Go to the previous subcommand."
  (interactive)
  (when (> (length noman--history) 1)
    (pop noman--history) ;; pop current command
    (let ((previous-cmd (pop noman--history)))
      (message previous-cmd)
      (noman previous-cmd))))

(defun noman--follow-link (button)
  "Follow a link from BUTTON in a noman buffer."
  (let ((subcommand (button-label button)))
    (noman (format "%s %s" noman--last-command subcommand))))

(defun noman--exec (cmd suffix buffer)
  "Execute CMD with the help SUFFIX and place the results in BUFFER."
  (call-process "sh" nil buffer nil "-c" (concat cmd " " suffix)))

(defun noman--make-aws-button (line)
  "Parse the string LINE for an aws-style command."
  (let ((first-match
	 (string-match
	  "^ +o +\\([A-Za-z0-9\\-]+\\)$"
	  line)))
    (when first-match
      (let ((beg (match-beginning 1)) (end (match-end 1)))
	(make-button
	 (+ (line-beginning-position) beg)
	 (+ (line-beginning-position) end)
	 'action #'noman--follow-link)))))

(defvar noman-parsing-functions
  '(("aws" . noman--make-aws-button))
  "Custom parsing functions to use for specific commands.
Each function should take a single string containing a line of text,
and return a button, or nil.")

(defun noman--make-default-button (line)
  "Parse the string LINE for a default command."
  (when (string-prefix-p "  " line)
    (let ((first-match
	   (string-match
	    "^ +\\([A-Za-z]+[A-Za-z0-9\\-]+\\):* \\{2\\}.*$"
	    line)))
      (when first-match
	(let ((beg (match-beginning 1)) (end (match-end 1)))
	  (make-button
	   (+ (line-beginning-position) beg)
	   (+ (line-beginning-position) end)
	   'action #'noman--follow-link))))))

(defun noman--get-button-func (cmd)
  "Gets the function to use for parsing subcommands for the given CMD."
  (let ((func
	 (cdr (assoc (nth 0 (split-string cmd " ")) noman-parsing-functions))))
    (if func
	func
      #'noman--make-default-button)))

(defun noman--make-buttons (buffer cmd)
  "Iterates over the lines in a BUFFER, parsing subcommands for CMD as buttons."
  (let ((buttons (list))
	(button-func (noman--get-button-func cmd)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (let ((max-lines (count-lines (point-min) (point-max))))
	(while (< (line-number-at-pos (point)) (+ max-lines 1))
	  (when-let ((current-line-string
		      (buffer-substring-no-properties
		       (line-beginning-position) (line-end-position)))
		     (button (apply button-func (list current-line-string))))
	    (push button buttons))
	  (forward-line))))
    buttons))

(defvar-keymap noman-mode-map
  "m" #'noman-menu
  "g" #'noman-menu
  "G" #'noman
  "q" #'quit-window
  "n" #'next-line
  "p" #'previous-line
  "l" #'noman-back)

(define-derived-mode noman-mode special-mode "noman"
  "A mode for browsing command line help.")

(defun noman (cmd)
  "Attempt to parse command line help for the command CMD.

Noman (no-man) has similar keybindings to man:

g/m  -  jump to a subcommand
G    -  view help for a different command
l    -  go back to the last subcommand"
  (interactive (list (read-shell-command "Command: ")))
  (setq noman--last-command cmd)
  (push cmd noman--history)
  (let* ((buffer (get-buffer-create (format "*noman %s*" cmd)))
         (cmdprefix (car (split-string cmd)))
         (cmdtype (string-trim (shell-command-to-string
                                (concat "command -V " cmdprefix))))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (cond
       ((string-suffix-p " is a shell builtin" cmdtype)
        (noman--exec "help -m" cmd t))
       ((string-suffix-p " not found" cmdtype)
        (user-error "Command '%s' not found" cmdprefix))
       (t
        (unless (= (noman--exec cmd "--help" '(t nil)) 0)
          (erase-buffer)
          (noman--exec cmd "help" '(t nil))
          (replace-regexp-in-region "." "" (point-min) (point-max)))
        (when-let ((versioninfo
                    (save-excursion
                      (with-temp-buffer
                        (unless (= (noman--exec cmdprefix "--version" '(t nil))
                                   0)
                          (erase-buffer)
                          (noman--exec cmdprefix "version" '(t nil))
                          (replace-regexp-in-region "." ""
                                                    (point-min)
                                                    (point-max)))
                        (indent-code-rigidly (point-min) (point-max) 4)
                        (buffer-string))))
                   (versioninfo-p (not (string= (string-trim versioninfo) ""))))
          (goto-char (point-max))
          (insert "\nIMPLEMENTATION\n")
          (insert versioninfo))))
      (ansi-color-apply-on-region (point-min) (point-max))
      (read-only-mode t)
      (noman-mode)
      (setq noman--buttons (noman--make-buttons buffer cmd))
      (goto-char (point-min))
      (display-buffer buffer))))

(provide 'noman)
;;; noman.el ends here
