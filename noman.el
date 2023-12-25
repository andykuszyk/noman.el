;;; noman.el --- read command line help without a man page.
;;; Commentary:

;;; Code:
(setq display-buffer-alist '(("noman" display-buffer-same-window)))

(setq noman--last-command "")

(setq noman--buttons '())

(setq noman--history '())

(defun noman-menu (label)
  (interactive (list (completing-read "Sub-command: " (cl-mapcar #'button-label noman--buttons))))
  (catch 'noman--button-found
    (dolist (button noman--buttons)
      (when (string= label (button-label button))
	(button-activate button)
	(throw 'noman--button-found t)))))

(defun noman-back ()
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
  (let ((cmd-and-options (split-string cmd " ")))
    (apply 'call-process
	   (append
	    (list (nth 0 cmd-and-options) nil buffer nil)
	    (nthcdr 1 cmd-and-options)
	    (list suffix)))))

(defun noman (cmd)
  "Attempt to parse comand line help for the command CMD."
  (interactive "MCommand: ")
  (setq noman--last-command cmd)
  (setq noman--buttons '())
  (push cmd noman--history)
  (let ((buffer (get-buffer-create "noman")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (unless (= (noman--exec cmd "--help" buffer) 0)
	(erase-buffer)
	(noman--exec cmd "help" buffer)
	(replace-regexp-in-region "." "" (point-min) (point-max)))
      (read-only-mode t)
      (goto-char (point-min))
      (let ((max-lines (count-lines (point-min) (point-max))))
	(while (< (line-number-at-pos (point)) (+ max-lines 1))
	  (let ((current-line-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
	    (when (string-prefix-p "  " current-line-string)
	      (let ((first-match (string-match "^ +\\([A-Za-z0-9\\-]+\\) \\{2\\}.*$" current-line-string)))
		(when first-match
		  (let ((beg (match-beginning 1)) (end (match-end 1)))
		    (setq noman--buttons (append noman--buttons (list (make-button (+ (line-beginning-position) beg) (+ (line-beginning-position) end) 'action #'noman--follow-link)))))))))
	  (forward-line)))
      (goto-char (point-min))
      (local-set-key (kbd "m") #'noman-menu)
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "n") #'next-line)
      (local-set-key (kbd "p") #'previous-line)
      (local-set-key (kbd "l") #'noman-back)
      (display-buffer buffer))))

(provide 'noman)
;;; noman.el ends here
