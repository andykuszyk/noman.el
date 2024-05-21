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

(require 'ert)
(add-to-list 'load-path "../")
(require 'noman)
(require 'f)

(defun make-kubectl ()
  (let ((name (make-temp-file "kubectl")))
    (f-write-text "#!/bin/bash
if [[ \"$1\" == \"--help\" ]]; then
    echo '
kubectl controls the Kubernetes cluster manager.

 Find more information at: https://kubernetes.io/docs/reference/kubectl/

Basic Commands (Beginner):
  create          Create a resource from a file or from stdin
  expose          Take a replication controller, service, deployment or pod and expose it as a new Kubernetes service
  run             Run a particular image on the cluster
  set             Set specific features on objects
'
fi

if [[ \"$1\" == \"create\" ]]; then
    echo '
Create a resource from a file or from stdin.

 JSON and YAML formats are accepted.

Examples:
  # Create a pod using the data in pod.json
  kubectl create -f ./pod.json
  
  # Create a pod based on the JSON passed into stdin
  cat pod.json | kubectl create -f -
  
  # Edit the data in registry.yaml in JSON then create the resource using the edited data
  kubectl create -f registry.yaml --edit -o json

Available Commands:
  clusterrole           Create a cluster role
  clusterrolebinding    Create a cluster role binding for a particular cluster role
  configmap             Create a config map from a local file, directory or literal value
'
fi

if [[ \"$1\" == \"run\" ]]; then
    echo '
Create and run a particular image in a pod.

Examples:
  # Start a nginx pod
  kubectl run nginx --image=nginx
'
fi
" 'utf-8-emacs name)
    (chmod name #o777)
    (message name)
    name))

(defun make-aws ()
  (let ((name (make-temp-file "aws")))
    (f-write-text "#!/bin/bash
if [[ \"$1\" == \"--help\" ]]; then
    exit 252
fi
if [[ \"$1\" == \"help\" ]]; then
    echo '
AWS()                                                                    AWS()

NAME
       aws -

DESCRIPTION
       The  AWS  Command  Line  Interface is a unified tool to manage your AWS
       services.

AVAILABLE SERVICES
       o accessanalyzer

       o account

       o acm

       o acm-pca
'
fi
if [[ \"$1\" == \"rds\" && \"$2\" == \"help\" ]]; then
echo '
RDS()                                                                    RDS()

NAME
       rds -

DESCRIPTION
       Amazon  Relational  Database Service (Amazon RDS) is a web service that
       makes it easier to set up, operate, and scale a relational database  in
       the  cloud.  It provides cost-efficient, resizeable capacity for an in-
       dustry-standard relational database and manages common database  admin-
       istration tasks, freeing up developers to focus on what makes their ap-
       plications and businesses unique.

AVAILABLE COMMANDS
       o add-option-to-option-group

       o add-role-to-db-cluster

       o add-role-to-db-instance
'
fi
" 'utf-8-emacs name)
    (chmod name #o777)
    (message name)
    name))

(defun noman--test-setup ()
  (setq noman-reuse-buffers nil)
  (kill-matching-buffers "\\*noman.*" nil t))

(defun count-buttons ()
  (goto-char (point-min))
  (let ((count 0))
    (while (forward-button 1 nil nil t)
      (setq count (+ count 1)))
    count))

(ert-deftest noman-should-parse-aws ()
  (noman--test-setup)
  (let* ((aws (make-aws))
	 (buffer (format "*noman %s*" aws)))
    (add-to-list 'noman-parsing-functions `(,aws . noman--make-aws-button))
    (noman aws)
    (with-current-buffer (get-buffer buffer)
      (should (string-equal buffer (buffer-name)))
      (should (> (point-max) 0))
      (should
       (string-match-p
	(regexp-quote "The  AWS  Command  Line  Interface is a unified tool")
	(buffer-substring-no-properties (point-min) (point-max))))
      (should (= (count-buttons) 4)))))

(ert-deftest noman-should-parse-aws-subcommands ()
  (noman--test-setup)
  (let* ((aws (make-aws))
	 (buffer (format "*noman %s*" aws))
	 (rds-buffer (format "*noman %s rds*" aws)))
    (noman aws)
    (with-current-buffer (get-buffer buffer)
      (noman-menu "rds")
      (with-current-buffer (get-buffer rds-buffer)
	(should
	 (string-match-p
	  (regexp-quote "Amazon  Relational  Database Service (Amazon RDS).")
	  (buffer-substring-no-properties (point-min) (point-max))))
	(should (= (count-buttons) 3))))))

(ert-deftest noman-should-parse-kubectl ()
  (noman--test-setup)
  (let* ((kubectl (make-kubectl))
	 (buffer (format "*noman %s*" kubectl)))
    (noman kubectl)
    (with-current-buffer (get-buffer buffer)
      (should (string-equal buffer (buffer-name)))
      (should (> (point-max) 0))
      (should
       (string-match-p
	(regexp-quote "kubectl controls the Kubernetes")
	(buffer-substring-no-properties (point-min) (point-max))))
      (should (= (count-buttons) 4)))))

(ert-deftest noman-should-parse-kubectl-subcommands ()
  (noman--test-setup)
  (let* ((kubectl (make-kubectl))
	 (buffer (format "*noman %s*" kubectl))
	 (create-buffer (format "*noman %s create*" kubectl)))
    (noman kubectl)
    (with-current-buffer (get-buffer buffer)
      (noman-menu "create")
      (with-current-buffer (get-buffer create-buffer)
	(should
	 (string-match-p
	  (regexp-quote "Create a resource from a file or from stdin.")
	  (buffer-substring-no-properties (point-min) (point-max))))
	(should (= (count-buttons) 3))))))

(ert-deftest noman-switching-buffers-should-retain-base-command ()
  (noman--test-setup)
  (let* ((kubectl (make-kubectl))
	 (kubectl-buffer-name (format "*noman %s*" kubectl))
	 (kubectl-run-buffer-name (format "*noman %s run*" kubectl)))
    (noman kubectl)
    (with-current-buffer (get-buffer kubectl-buffer-name)
      (noman-menu "create"))
    (with-current-buffer (get-buffer kubectl-buffer-name)
      (noman-menu "run"))
    (should (get-buffer kubectl-run-buffer-name))
    (with-current-buffer (get-buffer kubectl-run-buffer-name)
      (should
       (string-match-p
	"Create and run a particular image in a pod."
	(buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest noman-with-prefix-arg-allows-shell-built-ins ()
  (let* ((type-buffer-name "*noman type*")
	 (noman-shell-file-name "/bin/bash"))
    (noman--test-setup)
    (universal-argument)
    (noman "type")
    (should (get-buffer type-buffer-name))
    (with-current-buffer (get-buffer type-buffer-name)
      (should
       (string-match-p
	"type - Display information about command type."
	(buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest noman-without-prefix-arg-does-not-allow-built-in ()
  (let* ((type-buffer-name "*noman type*"))
    (noman--test-setup)
    (noman "type")
    (should (get-buffer type-buffer-name))
    (with-current-buffer (get-buffer type-buffer-name)
      (should
       (not
	(string-match-p
	 "type - Display information about command type."
	 (buffer-substring-no-properties (point-min) (point-max))))))))

(provide 'test-noman)
;;; test-noman.el ends here
