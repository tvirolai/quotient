;;; quotient.el --- A library for generating random quotes using a text corpus -*- lexical-binding: t -*-

;; Copyright (C) 2025 Tuomo Virolainen

;; Author: Tuomo Virolainen <tvirolai@soittakaaparanoid.mail.kapsi.fi>

;; URL: https://github.com/tvirolai/quotient
;; Keywords: lisp
;; Package-Requires: ((emacs "25.1") (compat "30.1.0.1"))
;; Version: 0.1

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
;; Quotient is a simple library for generating and displaying randomly generated
;; quotes (excerpts) from corpus files.

;;; Code:

(require 'compat)

(defgroup quotient ()
  "Settings for Quotient."
  :group 'editing)

;; User settings.

(defcustom quotient-corpus nil
  "Path to your corpus file, inside the Emacs directory."
  :type 'string
  :group 'quotient)

(defcustom quotient-excerpt-length 4
  "The length, in rows, of the quotes (excerpts) to generate."
  :type 'number
  :group 'quotient)

(defcustom quotient-scratch-buffer-name "*scratch*"
  "The name of the scratch buffer."
  :type 'string
  :group 'quotient)

;; Code

(defun quotient--slurp (file)
  "Read the contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun quotient-read-corpus (corpus)
  "Read CORPUS file, return a list of rows."
  (let ((corpus-file-expanded (expand-file-name corpus)))
    (if (file-exists-p corpus-file-expanded)
        (split-string
         (quotient--slurp corpus-file-expanded) "\n" t)
      (message "Corpus file not found."))))

(defun quotient-get-excerpt (rows)
  "Generate and return an excerpt from ROWS."
  (let* ((count (length rows))
         (start-index (random (- count quotient-excerpt-length)))
         (res (seq-subseq rows start-index (+ start-index quotient-excerpt-length))))
    (if (seq-filter (lambda (x)
                      (string-match-p "^\\(?:0\\|[1-9][0-9]*\\)" x))
                    res)
        (quotient-get-excerpt rows)
      (mapconcat #'string-trim-left res "\n"))))

;;;###autoload
(defun quotient-generate-excerpt (&optional format)
  "Generate and return an excerpt.
Optional argument FORMAT can be `comment' (commented out with semicolons) or
`eshell' (with two line breaks added)."
  (if quotient-corpus
      (let ((rows (quotient-read-corpus quotient-corpus)))
        (if (>= (length rows)
                quotient-excerpt-length)
            (let ((excerpt (quotient-get-excerpt rows)))
              (if format
                  (cond ((eq format 'comment) (concat  ";; " (string-replace "\n" "\n;; " excerpt)))
                        ((eq format 'eshell) (concat excerpt "\n\n"))
                        (t excerpt))
                excerpt))
          (message "Corpus file contains less rows than desired excerpt length.")))
    (message "Corpus file not configured.")))

;;;###autoload
(defun quotient-display-random-excerpt ()
  "Generate an excerpt and view it in the minibuffer."
  (interactive)
  (message (quotient-generate-excerpt)))

(defun quotient-wipe-scratch-message ()
  "Remove existing scratch message, if any."
  (when (get-buffer quotient-scratch-buffer-name)
    (with-current-buffer quotient-scratch-buffer-name
      (goto-char (point-min))
      (let ((excerpt-char (cond ((derived-mode-p 'lisp-interaction-mode) (string-to-char ";"))
                              ((derived-mode-p 'org-mode) ?#)
                              (t ?/)))
            (next-line-add-newlines-orig-val next-line-add-newlines))
        (setq next-line-add-newlines t)
        (while (eq excerpt-char (char-after))
          (forward-line))
        (delete-region (point-min) (point))
        (setq next-line-add-newlines next-line-add-newlines-orig-val)))))

(defun quotient-scratch-message-insert (excerpt-text)
  "Insert EXCERPT-TEXT into the top of the scratch buffer."
  (when (get-buffer quotient-scratch-buffer-name)
    (with-current-buffer quotient-scratch-buffer-name
      (goto-char (point-min))
      (insert excerpt-text "\n")
      (goto-char (point-min))
      (forward-line quotient-excerpt-length)
      (comment-region (point-min) (point)))))

;;;###autoload
(defun quotient-set-scratch-message ()
  "Generate a new excerpt and set it to the *scratch* buffer."
  (interactive)
  (save-excursion
    (quotient-wipe-scratch-message)
    (quotient-scratch-message-insert (quotient-generate-excerpt))))

(provide 'quotient)

;;; quotient.el ends here
