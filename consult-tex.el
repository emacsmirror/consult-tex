;;; consult-tex.el --- Consult powered completion for tex -*- lexical-binding: t -*-

;; Copyright (C) 2023 Titus Pinta

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Titus Pinta
;; Maintainer: Titus Pinta <titus.pinta@gmail.com>
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (consult "0.35"))
;; Homepage: https://gitlab.com/titus.pinta/consult-TeX
;; Keywords: consult, tex, latex


;; This file is =NOT= part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; This package provides consult based commands to work with tex citations and
;; references. Consult enables the preview of the text near the definition of
;; a label in order to facilitate the selection of the correct reference, based
;; on information obtained from the context.

;; The commands provided are
;;  'consult-tex-reference' - Use consult to find a reference
;;  'consult-tex-insert-reference'  Use consult to insert a reference
;;  'consult-tex-citation' - Use consult to find a citation
;;  'consult-tex-insert-citation'  Use consult to insert a citation

;; The variable provided is
;;  'consult-tex-version' - Holds the current version

;;; TODOs:
;; parse bibtex items
;; have a function to get the bib file name
;; use non braking space
;;  better readme with images
;; get the  bib filename if narrow or if tex-master
;; if the point is already in a cite block, add the citation to this block
;; TODO when inserting a reference use eqref if the point is in a equation

;;; Code:
(require 'consult)

(defvar consult-tex-version "0.3.0"
  "Current version of consult-tex.")

;;;###autoload
(defun consult-tex-reference ()
  "Use consult to find a reference."
  (interactive)
  (push-mark (point) t)
  (when (fboundp 'evil--jumps-push) (evil--jumps-push))
  (goto-char (consult-tex--find-reference)))


;;;###autoload
(defun consult-tex-insert-reference ()
  "Use consult to insert a reference."
  (interactive)
  (when (or (eq (char-before) ? ) (eq (char-before) ?~)) (delete-char -1))
  (insert (format "~\\ref{%s}"
		  (save-excursion
		    (goto-char (consult-tex--find-reference))
		    (re-search-forward "\\(.*\\)}" nil t)
		    (match-string-no-properties 1)))))


(defun consult-tex--find-reference ()
  "Internal function for \\='consult-tex-reference'."
  (interactive)
  (let ((refs ()))
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "\\\\label{\\(.*\\)}" nil t)
	(goto-char (match-beginning 1))
	(push (propertize (match-string 1) 'consult-location
			  (cons (point-marker) (line-number-at-pos)))
	      refs)
	(goto-char (match-end 0)))
      (setq refs (seq-uniq (reverse refs) #'string=)))
    ;; The next part sorts the completion candidates such that the first item
    ;; is the first from above the point.
    ;; TODO add a flag here to control this behavior
    (let ((head refs)
	  (old refs))
      (while
	  (and refs (< (cdr (get-text-property 0 'consult-location (car refs)))
		       (line-number-at-pos)))
	(setq old refs)
	(setq refs (cdr refs)))
      (setf (cdr old) nil)
      (setq refs (reverse (append refs head))))
    (consult--read
     refs
     :prompt "References:"
     :annotate (consult--line-prefix)
     :category 'consult-location
     :sort nil
     :require-match t
     :lookup #'consult--lookup-location
     :history '(:input consult--line-history)
     :add-history (thing-at-point 'symbol)
     :default (car refs)
     :state (consult--jump-preview))))


;;;###autoload
(defun consult-tex-citation ()
  "Use consult to find a citation."
  (interactive)
  (push-mark (point) t)
  (when (fboundp 'evil--jumps-push) (evil--jumps-push))
  (let ((m (consult-tex--find-citation))
	(ref-file (consult-tex--find-bibfile)))
    (find-file ref-file)
    (goto-char m)))


;;;###autoload
(defun consult-tex-insert-citation ()
  "Use consult to insert a citation."
  (interactive)
  (let ((m (consult-tex--find-citation))
	(ref-file (consult-tex--find-bibfile))
	text)
    (with-temp-buffer
      (insert-file-contents ref-file)
      (goto-char (1- m))
      (re-search-forward "{\\(.*\\)," nil t)
      (setq text (match-string 1)))
    (when (eq (char-before) ? ) (delete-char -1))
    (if (let* ((start (save-excursion (search-backward "\\cite{")))
	       (end (save-excursion (goto-char start) (search-forward "}"))))
	  (and (>= (point) start) (<= (point) end)))
	(progn (search-forward "}") (backward-char) (insert ?, text))
      (insert (format "~\\cite{%s}" text)))))


(defun consult-tex--find-citation ()
  "Internal function for \\='consult-tex-citation'."
  (interactive)
  (let ((bibs ())
	(ref-file (consult-tex--find-bibfile)))
    (with-temp-buffer
      (insert-file-contents ref-file)
      (goto-char 0)
      (while (re-search-forward "@.*{\\(.*\\)," nil t)
	(let (bibitem-start bibitem-end bibitem)
	  (setq bibitem-start (match-beginning 0))
	  (goto-char (1- (match-beginning 1)))
	  (save-excursion
	    (forward-sexp)
	    (setq bibitem-end (point)))
	  (setq bibitem
		(buffer-substring-no-properties bibitem-start bibitem-end))
	  (push (propertize (consult-tex--parse-bibitem bibitem)
			    'consult-location (cons (point-marker)
						    (line-number-at-pos)))
		bibs)
	  (forward-sexp))
	(goto-char (match-end 0)))
      (setq bibs (reverse bibs))
      (marker-position
       (consult--read
	bibs
	:prompt "Citations:"
	:annotate (consult--line-prefix)
	:category 'consult-location
	:sort nil
	:require-match t
	:lookup #'consult--lookup-location
	:history '(:input consult--line-history)
	:add-history (thing-at-point 'symbol)
	:default (car bibs)
	:state (consult--jump-preview))))))


(defun consult-tex--find-bibfile ()
  "Find the bib file in the current buffer."
  (interactive)
  (save-excursion
    (goto-char 0)
    (re-search-forward "\\\\bibliography{\\(.*\\)}" nil t))
  (format "%s.bib" (match-string-no-properties 1)))


(defun consult-tex--parse-bibitem (text)
  "Parse TEXT as a bibitem and return a string representation."
  (let (auth-and-title (data (match-data)))
    (with-temp-buffer
      (insert text)
      (dolist (entry '(author title) auth-and-title)
	(goto-char 0)
	(message (format "%s[[:space:]]*=[[:space:]]*{?" entry))
	(if (re-search-forward (format "%s[[:space:]]*=[[:space:]]*{?" entry)
			       nil t)
	    (progn
	      (goto-char (1- (match-end 0)))
	      (forward-sexp)
	      (push (buffer-substring-no-properties (match-end 0) (1- (point)))
		    auth-and-title))
	  (push (format "No %s" entry) auth-and-title)))
      (setf (car auth-and-title)
	    (propertize (car auth-and-title) 'face '(:slant italic)))
      (set-match-data data)
      (format "%s %s" (cadr auth-and-title) (car auth-and-title)))))

;; TODO here
(defun consult-tex-uncited-items ()
  "Message the bib items that are not cited."
  (interactive)
  (message "Uncited items")
  (let ((bibs ())
	(uncited ())
	(ref-file (consult-tex--find-bibfile)))
    (with-temp-buffer
      (insert-file-contents ref-file)
      (goto-char 0)
      (while (re-search-forward "@.*{\\(.*\\)," nil t)
	(push
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 bibs)))
    (dolist (bib bibs)
      (save-excursion
	(goto-char 0)
	(unless (re-search-forward bib nil t)
	  (message bib)
	  (push bib uncited))))
    (length uncited)))

(defun consult-tex-math-swiper ()
  "Like swiper, but only for math."
  (interactive)
  (let ((math '()))
    (save-excursion
      (goto-char 0)
      (while
	  (re-search-forward
	   "\\\\begin{equation[*]?}\\(\\(.\\|\n\\)*?\\)\\\\end{equation[*]?}"
	   nil t))
      (push (match-string 1) math))
    (dolist (line math)
      (insert line))))

(provide 'consult-tex)
;;; consult-tex.el ends here
