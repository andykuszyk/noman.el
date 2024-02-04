;;; test-noman.el --- tests for the noman package    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <emacs@akuszyk.com>
;; Keywords: docs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for the noman package.

;;; Code:

(add-to-list 'load-path "../")
(require 'noman)
(require 'f)

(defun make-kubectl ()
  (let ((name (make-temp-file "kubectl")))
    (f-write-text "
#!/bin/bash
echo '
kubectl controls the Kubernetes cluster manager.

 Find more information at: https://kubernetes.io/docs/reference/kubectl/

Basic Commands (Beginner):
  create          Create a resource from a file or from stdin
  expose          Take a replication controller, service, deployment or pod and expose it as a new Kubernetes service
  run             Run a particular image on the cluster
  set             Set specific features on objects
'" 'utf-8-emacs name)
    (chmod name #o777)
    (message name)
    name))

(ert-deftest noman-should-parse-kubectl ()
  (let* ((kubectl (make-kubectl))
	(buffer (format "*noman %s*" kubectl)))
    (noman kubectl)
    (with-current-buffer (get-buffer buffer)
      (should (string-equal buffer (buffer-name)))
      (should (> (point-max) 0))
      (should
       (string-match-p
	(regexp-quote "kubectl controls the Kubernetes")
	(buffer-substring-no-properties (point-min) (point-max)))))))

(provide 'test-noman)
;;; test-noman.el ends here
