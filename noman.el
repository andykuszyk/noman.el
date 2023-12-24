;;; noman.el --- read command line help without a man page.
;;; Commentary:

;;; Code:
(setq display-buffer-alist '(("noman" display-buffer-same-window)))

(defun noman (cmd)
  "Attempt to parse comand line help for the command CMD."
  (interactive "Mcommand: ")
  (let ((buffer (get-buffer-create "noman")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer buffer)
      (call-process cmd nil buffer nil "--help")
      (read-only-mode t)
      (goto-char (point-min)))
    (display-buffer buffer)))

(provide 'noman)
;;; noman.el ends here
