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

;;; TODOs:

;;; Code:
(require 'consult)

;;;###autoload
(defun consult-tex-reference ()
  "Use consult to find a reference."
  (interactive)
  (push-mark)
  (goto-char (consult-tex--find-reference)))


;;;###autoload
(defun consult-tex-insert-reference ()
  "Use consult to insert a reference."
  (interactive)
  (insert (format "\\ref{%s}"
		  (save-excursion
		    (goto-char (consult-tex--find-reference))
		    (re-search-forward "\\(.*\\)}" nil t)
		    (match-string-no-properties 1)))))


(defun consult-tex--find-reference ()
  "Internal function for 'consult-tex-reference'."
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
  (push-mark)
  (let ((m (consult-tex--find-citation))
	ref-file)
    (save-excursion
      (goto-char 0)
      (re-search-forward "\\\\bibliography{\\(.*\\)}" nil t))
    (setq ref-file (format "%s.bib" (match-string-no-properties 1)))
    (find-file ref-file)
    (goto-char m)))


;;;###autoload
(defun consult-tex-insert-citation ()
  "Use consult to insert a citation."
  (interactive)
  (let ((m (consult-tex--find-citation))
	ref-file
	text)
    (save-excursion
      (goto-char 0)
      (re-search-forward "\\\\bibliography{\\(.*\\)}" nil t))
    (setq ref-file (format "%s.bib" (match-string-no-properties 1)))
    (with-temp-buffer
      (insert-file-contents ref-file)
      (goto-char (1- m))
      (re-search-forward "{\\(.*\\)," nil t)
      (setq text (match-string 1)))
    (insert (format "\\cite{%s}" text))))


(defun consult-tex--find-citation ()
  "Internal function for 'consult-tex-citation'."
  (interactive)
  (let ((bibs ())
	ref-file)
    (save-excursion
      (goto-char 0)
      (re-search-forward "\\\\bibliography{\\(.*\\)}" nil t))
    (setq ref-file (format "%s.bib" (match-string-no-properties 1)))
    (with-temp-buffer
      (bibtex-mode)
      (font-lock-mode 1)
      (font-lock-update)
      (line-number-mode 1)
      (insert-file-contents ref-file)
      (goto-char 0)
      (while (re-search-forward "@.*{\\(.*\\)," nil t)
	(goto-char (match-beginning 1))
	(push (propertize (match-string 1) 'consult-location
			  (cons (point-marker) (line-number-at-pos)))
	      bibs)
	(setq bibs
	      (sort bibs
		    (lambda (a b)
		      (< (car (get-text-property 0 'consult-location a))
			 (car (get-text-property 0 'consult-location b))))))
	(goto-char (match-end 0)))
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

(provide 'consult-tex)
;;; consult-tex.el ends here
