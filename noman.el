;;; noman.el --- read command line help without a man page.
;;; Commentary:

;;; Code:
(setq display-buffer-alist '(("noman" display-buffer-same-window)))

(setq noman--last-command "")

(setq noman--buttons '())

(defun noman-menu (label)
  (interactive (list (completing-read "prompt: " (cl-mapcar #'button-label noman--buttons))))
  (message label))

(defun noman--follow-link (button)
  "Follow a link from BUTTON in a noman buffer."
  (let ((subcommand (button-label button)))
    (noman (format "%s %s" noman--last-command subcommand))))

(defun noman (cmd)
  "Attempt to parse comand line help for the command CMD."
  (interactive "Mcommand: ")
  (setq noman--last-command cmd)
  (setq noman--buttons '())
  (let ((buffer (get-buffer-create "noman")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (let ((cmd-and-options (split-string cmd " ")))
	(apply 'call-process
	       (append
		(list (nth 0 cmd-and-options) nil buffer nil)
		(nthcdr 1 cmd-and-options)
		(list "--help"))))
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
      (display-buffer buffer))))

(provide 'noman)
;;; noman.el ends here
