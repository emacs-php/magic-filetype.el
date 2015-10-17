;;; vim-filetype-test.el --- test for vim-filetype -*- mode: emacs-lisp; lexical-binding: t -*-

;; Copyright (C) 2015 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 20 Jun 2015

;;; Commentary:

;; This file is NOT part of GNU Emacs.

;;; Code:
(require 'magic-filetype)

(ert-deftest vim-filetype/test\#test-regexp ()
  (let ((data
         (list
          (list :expected "javascript"
                :header "// vim:set ft=javascript:"))))
    (mapc
     (lambda (d)
       (let ((expected (plist-get d :expected))
             (header   (plist-get d :header)))
         (should (string= expected
                          (cadr (s-match magic-filetype-vim-filetype-line-re header))
                          ))))
     data)))

;;; vim-filetype-test.el ends here
