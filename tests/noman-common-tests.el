;;; noman-common-tests.el --- common test functionality for the noman package    -*- lexical-binding: t; -*-

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

(defmacro noman-test (cli-name cli-script)
  "Define test cases for using noman with a program named CLI-NAME.
The implementation is defined by CLI-SCRIPT.  CLI-SCRIPT should be a Bash 
script, or a script with a suitable hashbang, which will be invoked in place of 
CLI-NAME during test execution."
  (let ((cli-file-name (make-symbol "cli-file-name")))
    `(progn
       (defun ,(intern (format "noman-make-%s" cli-name)) ()
	 (let ((,cli-file-name (make-temp-file ,cli-name)))
	   (f-write-text ,cli-script 'utf-8-emacs ,cli-file-name)
	   (chmod ,cli-file-name #o777)
	   ,cli-file-name))
       (ert-deftest ,(intern (format "noman-should-parse-%s" cli-name)) ()
	 )
       (ert-deftest
	   ,(intern (format "noman-should-parse-%s-subcommands" cli-name)) ()
	 ))))

(provide 'noman-common-tests)
