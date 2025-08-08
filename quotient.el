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

(require 'scratch-message)

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

;;

(defvar quotient-quotes '())
(defvar quotient-rows '()
  "This holds the raw data read from the corpus.")
(defvar quotient-current-quote nil)

(defun quotient-slurp (file)
  "Read the contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun quotient-read-corpus ()
  "Read the corpus into memory, split into rows."
  (let ((corpus-file-expanded (expand-file-name quotient-corpus user-emacs-directory)))
    (if (file-exists-p corpus-file-expanded)
        (setq quotient-rows
         (split-string
          (quotient-slurp corpus-file-expanded) "\n" t))
      (message "Corpus file not found."))))

(when quotient-corpus
  (quotient-read-corpus))

(add-variable-watcher 'quotient-corpus
                      (lambda (symbol newval operation where)
                        (quotient-read-corpus)))

;;;###autoload
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
(defun quotient-generate-quote ()
  "Generate and return a quote."
  (if quotient-rows
      (quotient-get-quote quotient-rows)
    (message "Corpus file not found.")))

;;;###autoload
(defun quotient-display-random-quote ()
  "Generate a quote and view it in the minibuffer."
  (interactive)
  (message (quotient-generate-quote)))

;;;###autoload
(defun quotient-set-scratch-message ()
  "Generate a new quote and set it to the *scratch* buffer."
  (interactive)
  (setq initial-scratch-message "")
  (let ((quote (quotient-generate-quote)))
    (scratch-message-insert quote)))

;;;###autoload
(defun quotient-set-eshell-banner-message ()
  "Generate a new quote and set it to the eshell banner."
  (let ((props (text-properties-at 0 eshell-banner-message)))
    (setq eshell-banner-message
          (propertize (concat (quotient-generate-quote) "\n\n")
                      'insert-in-front-hooks
                      (plist-get props 'insert-in-front-hooks)))))


(defun quotient-as-comment (quote)
  "Comments out the QUOTE."
  (concat  ";; " (string-replace "\n" "\n;; " quote)))

(provide 'quotient)

;;; quotient.el ends here
