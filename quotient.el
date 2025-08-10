;;; quotient.el --- A library for generating random quotes using a corpus. -*- lexical-binding: t -*-

;; Copyright (C) 2025 Tuomo Virolainen

;; Author: Tuomo Virolainen <tvirolai @ soittakaaparanoid.mail.kapsi.fi>

;; URL: https://github.com/tvirolai/quotient
;; Keywords: lisp
;; Version: 0.1
;; Package-Requires: ((scratch-message "20220209.2207"))

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

;;; Code:

(defgroup quotient ()
  "Settings for Quotient."
  :group 'editing)

;; User settings.

(defcustom quotient-corpus nil
  "Path to your corpus file, inside the Emacs directory."
  :type 'string
  :group 'quotient)

(defcustom quotient-quote-length 4
  "The length, in rows, of the quotes to generate."
  :type 'number
  :group 'quotient)

;; Code

(defvar scratch-buffer-name "*scratch*")

(setq next-line-add-newlines t)

(defun quotient-slurp (file)
  "Read the contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun quotient-read-corpus (corpus)
  "Read the CORPUS into memory, split into rows."
  (let ((corpus-file-expanded (expand-file-name corpus user-emacs-directory)))
    (if (file-exists-p corpus-file-expanded)
        (split-string
         (quotient-slurp corpus-file-expanded) "\n" t)
      (message "Corpus file not found."))))

(defun quotient-get-quote (rows)
  "Generate and return a quote from ROWS."
  (let* ((count (length rows))
         (start-index (random (- count quotient-quote-length)))
         (res (seq-subseq rows start-index (+ start-index quotient-quote-length))))
    (if (seq-filter (lambda (x)
                      (string-match-p "^\\(?:0\\|[1-9][0-9]*\\)" x))
                    res)
        (quotient-get-quote rows)
      (mapconcat 'identity res "\n"))))

;;;###autoload
(defun quotient-generate-quote (&optional format)
  "Generate and return a quote.
Optional argument FORMAT can be `comment' (commented out with semicolons) or
`eshell' (with two line breaks added)."
  (if quotient-corpus
      (let* ((rows (quotient-read-corpus quotient-corpus))
             (quote (quotient-get-quote rows)))
        (if format
            (cond ((eq format 'comment) (concat  ";; " (string-replace "\n" "\n;; " quote)))
                  ((eq format 'eshell) (concat quote "\n\n"))
                  (t quote))
          quote))
    (message "Corpus file not found.")))

;;;###autoload
(defun quotient-display-random-quote ()
  "Generate a quote and view it in the minibuffer."
  (interactive)
  (message (quotient-generate-quote)))

(defun quotient-wipe-scratch-message ()
  "Remove existing scratch message, if any."
  (when (get-buffer scratch-buffer-name)
    (with-current-buffer scratch-buffer-name
      (goto-char (point-min))
      (let ((quote-char (cond ((eq major-mode 'lisp-interaction-mode) (char-from-name "SEMICOLON"))
                              ((eq major-mode 'org-mode) ?#)
                              (t ?/))))
        (while (eq quote-char (char-after))
          (forward-line))
        (delete-region (point-min) (point))))))

(defun quotient-scratch-message-insert (quote)
  "Insert QUOTE into the top of the scratch buffer."
  (when (get-buffer scratch-buffer-name)
    (with-current-buffer scratch-buffer-name
      (goto-char (point-min))
      (insert (concat quote "\n"))
      (goto-char (point-min))
      (forward-line quotient-quote-length)
      (comment-region (point-min) (point)))))

;;;###autoload
(defun quotient-set-scratch-message ()
  "Generate a new quote and set it to the *scratch* buffer."
  (interactive)
  (save-excursion
    (let ((quote (quotient-generate-quote)))
      (quotient-wipe-scratch-message)
      (quotient-scratch-message-insert quote))))

(provide 'quotient)

;;; quotient.el ends here
