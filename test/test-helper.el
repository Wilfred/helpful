;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Wilfred Hughes

;; Author:  <me@wilfred.me.uk>

;;; Code:

(require 'ert)
(require 'f)

(let ((helpful-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path helpful-dir))

(require 'undercover)
(undercover "helpful.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))

;;; test-helper.el ends here
