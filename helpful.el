;;; helpful.el --- a better *help* buffer            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: help, lisp
;; Version: 0.1
;; Package-Requires: ((dash "2.12.0") (s "1.11.0"))

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

;; 

;;; Code:

(require 'help)
(require 'dash)
(require 's)

(defvar-local helpful--sym nil)

(defun helpful--buffer (symbol)
  "Return a buffer to show help for SYMBOL in."
  (let ((buf (get-buffer-create
              (format "*helpful: %s*" symbol))))
    (with-current-buffer buf
      (helpful-mode)
      (setq helpful--sym symbol))
    buf))

(defun helpful--heading (text)
  "Propertize TEXT as a heading."
  (propertize text 'face 'bold))

(defun helpful-update ()
  "Update the current *Helpful* buffer to the latest
state of the current symbol."
  (interactive)
  (let ((inhibit-read-only t)
        (start-pos (point)))
    (erase-buffer)
    (insert
     (format "Symbol: %s\n\n" helpful--sym)
     (helpful--heading "Documentation\n")
     (helpful--docstring helpful--sym))
    (goto-char start-pos)))

(defun helpful--skip-advice (docstring)
  "Remove mentions of advice from DOCSTRING."
  (let* ((lines (s-lines docstring))
         (relevant-lines
          (--drop-while (s-starts-with-p ":around advice:" it) lines)))
    (s-trim (s-join "\n" relevant-lines))))

(defun helpful--docstring (sym)
  "Get the docstring for SYM."
  (let* ((docstring (documentation sym))
         (docstring-with-usage (help-split-fundoc docstring sym)))
    (when docstring-with-usage
      (setq docstring (cdr docstring-with-usage))
      ;; Advice mutates the docstring, see
      ;; `advice--make-docstring'. Undo that.
      ;; TODO: Only do this if the function is adviced.
      (setq docstring (helpful--skip-advice docstring)))
    docstring))

(defun helpful (symbol)
  "Show Help for SYMBOL."
  (interactive
   (list (read (completing-read "Symbol: " obarray
                                nil nil nil nil
                                (symbol-name (symbol-at-point))))))
  (switch-to-buffer (helpful--buffer symbol))
  (helpful-update))

(define-derived-mode helpful-mode special-mode "Helpful")

(define-key helpful-mode-map (kbd "g") #'helpful-update)


(provide 'helpful)
;;; helpful.el ends here
