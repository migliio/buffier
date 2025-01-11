;;; buffier.el --- Core of the `buffier` package -*- lexical-binding: t -*-

;; Copyright (C) 2024  Claudio Migliorelli

;; Author: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; Maintainer: Claudio Migliorelli <claudio.migliorelli@mail.polimi.it>
;; URL: https://github.com/migliio/buffier
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This is the core file for the buffier package. The point of entry
;; is the `buffier-new-buffer' function, which streamlines buffer
;; creation, buffer renaming and mode enabling within it.  Then, users
;; can easily retrieve the active buffier buffers by means of
;; `buffier-get-buffers-list'. Then, they can select the buffier
;; buffer they are interested in and promptly switch to it.

;;; Code:

(defcustom buffier-default-buffer-name "untitled"
  "Default buffer name used in case there is no input provided by the user."
  :type 'string)

(defcustom buffier-default-prefix "buffier" "" :type 'string)

(defun buffier--get-major-modes ()
  "Utility function to get loaded major modes.

This function gets the major modes by leveraging on
`auto-mode-alist', which is a list of cons nodes of the form
\"(<name-pattern> . <major-mode-string>)\". However, there are
multiple entries per each major mode, thus we shoud filter
duplicates afterwards."
  (let ((modes
	 (seq-remove
	  (lambda (element)
	    (or (not (symbolp element))
		(eq element t)
		(eq element nil)))
	  (mapcar (lambda (alist-cons) (cdr alist-cons)) auto-mode-alist))))
    (delete-dups modes)))

(defun buffier--generate-new-buffer (input-buff-name)
  "Generate a new buffier buffer with name NAME.
In case NAME is the empty string, this function generates a
buffer with `buffier-default-buffer-name' as its name."
  (generate-new-buffer
   (format "*%s: %s*"
	   buffier-default-prefix
   (if (string-blank-p input-buff-name)
       buffier-default-buffer-name
     input-buff-name))))

(defun buffier--get-buffers-list ()
  "Get the list of openend buffier buffers.
The buffier buffers are obtained by means of filtering all
buffers matching `buffier-default-prefix'."
    (seq-filter
     (lambda (buffer)
       (string-match-p (format "\\*%s:" buffier-default-prefix) (buffer-name buffer)))
     (buffer-list)))

(defvar buffier-mode-history nil)

(defun buffier--mode-prompt ()
  "Generate the buffier prompt."
  (let ((default (car buffier-mode-history)))
    (completing-read
     (format-prompt "Select the MAJOR MODE to enable" default)
     (buffier--get-major-modes) nil :require-match nil 'buffier-mode-history default)))

;;;###autoload
(defun buffier-buffers ()
  "Get buffers opened with `buffier'.
These buffers are returned by means of minibuffer completion, in
tandem with the `major-mode' they were linked to. Upon selection,
switch to the chosen buffer."
  (interactive)
  (let* ((buffers (mapcar
                   (lambda (buffer)
                     (let ((buffer-name (buffer-name buffer))
                           (mode-name (symbol-name (buffer-local-value 'major-mode buffer))))
                       (cons (format "(%s,%s)" buffer-name mode-name) buffer)))
                   (buffier--get-buffers-list)))
         (selected-buffer (completing-read "Buffier buffers: " buffers)))
    (when selected-buffer
      (switch-to-buffer (cdr (assoc selected-buffer buffers))))))

;;;###autoload
(defun buffier-new-buffer ()
  "Create a new buffer and prompt the user for the major-mode to enable.

NOTE: as returned by auto-mode-alist cdr's, major modes are
strings. The `funcall' function takes a function canonical name
as input, then we should switch from string to canonical symbol
through `intern'."
  (interactive)
  (let* ((input-buff-name (read-string "Insert the buffer NAME: "))
	   (new-buff (buffier--generate-new-buffer input-buff-name))
	   (mode (buffier--mode-prompt))
	   (mode-symbol (intern mode)))
    (switch-to-buffer new-buff)
    (if (fboundp mode-symbol)
	  (funcall mode-symbol)
	(user-error "The selected mode doesn't exist or it is not lazily loaded"))
    (funcall (intern mode))
    new-buff))

;;; buffier.el ends here
