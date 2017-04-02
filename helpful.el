;;; helpful.el --- a better *help* buffer            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: help, lisp

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

(defvar-local helpful--sym nil)

(defun helpful--buffer (symbol)
  "Return a buffer to show help for SYMBOL in."
  (let ((buf (get-buffer-create
              (format "*helpful: %s*" symbol))))
    (with-current-buffer buf
      (helpful-mode)
      (setq helpful--sym symbol))
    buf))

(defun helpful-update ()
  "Update the current *Helpful* buffer to the latest
state of the current symbol."
  (interactive)
  (let ((inhibit-read-only t)
        (start-pos (point)))
    (erase-buffer)
    (insert
     (format "Symbol: %s\n\n" helpful--sym)
     "Documentation\n\n")
    (goto-char start-pos)))

(defun helpful (symbol)
  "Show Help for SYMBOL."
  (interactive
   (list (completing-read "Symbol: " obarray
                          nil nil nil nil
                          (symbol-name (symbol-at-point)))))
  (switch-to-buffer (helpful--buffer symbol))
  (helpful-update))

(define-derived-mode helpful-mode special-mode "Helpful")

(define-key helpful-mode-map (kbd "g") #'helpful-update)


(provide 'helpful)
;;; helpful.el ends here
