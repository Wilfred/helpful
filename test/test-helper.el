;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Wilfred Hughes

;; Author:  <me@wilfred.me.uk>

;;; Code:

(require 'ert)
(require 'f)

(let ((helpful-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path helpful-dir))

;;; test-helper.el ends here
