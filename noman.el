;;; noman.el --- read command line help without a man page -*- lexical-binding: t -*-

;; Copyright (C) 2024 Andy Kuszyk

;; Author: Andy Kuszyk <emacs@akuszyk.com>
;; URL: https://github.com/andykuszyk/noman.el
;; Version: 0.1

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

(defvar noman--buttons '()
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
  (if (> (length noman--history) 1)
    (let ((current-cmd (pop noman--history))
	  (previous-cmd (pop noman--history)))
      (message previous-cmd)
      (noman previous-cmd))))

(defun noman--follow-link (button)
  "Follow a link from BUTTON in a noman buffer."
  (let ((subcommand (button-label button)))
    (noman (format "%s %s" noman--last-command subcommand))))

(defun noman--exec (cmd suffix buffer)
  "Execute CMD with the help SUFFIX and place the results in BUFFER."
  (let ((cmd-and-options (split-string cmd " ")))
    (apply 'call-process
	   (append
	    (list (nth 0 cmd-and-options) nil buffer nil)
	    (nthcdr 1 cmd-and-options)
	    (list suffix)))))

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
pEach function should take a single string containing a line of text,
and return a button, or nil.")

(defun noman--make-default-button (line)
  "Parse the string LINE for a default command."
  (when (string-prefix-p "  " line)
    (let ((first-match
	   (string-match
	    "^ +\\([A-Za-z]+[A-Za-z0-9\\-]+\\) \\{2\\}.*$"
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

(defun noman (cmd)
  "Attempt to parse comand line help for the command CMD.

Noman (no-man) has similar keybindings to man:

g/m  -  jump to a subcommand
G    -  view help for a different command
l    -  go back to the last subcommand"
  (interactive "MCommand: ")
  (setq noman--last-command cmd)
  (setq noman--buttons '())
  (push cmd noman--history)
  (let ((buffer (get-buffer-create (format "*noman %s*" cmd)))
	(inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (unless (= (noman--exec cmd "--help" buffer) 0)
	(erase-buffer)
	(noman--exec cmd "help" buffer)
	(replace-regexp-in-region "." "" (point-min) (point-max)))
      (setq noman--buttons (noman--make-buttons buffer cmd))
      (ansi-color-apply-on-region (point-min) (point-max))
      (goto-char (point-min))
      (local-set-key (kbd "m") #'noman-menu)
      (local-set-key (kbd "g") #'noman-menu)
      (local-set-key (kbd "G") #'noman)
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "n") #'next-line)
      (local-set-key (kbd "p") #'previous-line)
      (local-set-key (kbd "l") #'noman-back)
      (read-only-mode t)
      (display-buffer buffer))))

(provide 'noman)
;;; noman.el ends here
