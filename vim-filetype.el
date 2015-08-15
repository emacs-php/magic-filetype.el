;;; vim-filetype.el --- Parse Vim-style filetype header
;;; vim: set ft=lisp:

;; Copyright (C) 2015 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 9 Aug 2015
;; Version: 0.0.1
;; Keywords: vim ft file magic-mode
;; Package-Requires: ((s "1.9.0") (dash "2.11.0"))

;; This file is NOT part of GNU Emacs.

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

;; `vim-filetype' parse Vim-style file header.
;; For example, in executable JavaScript(node) file is...

;;   #!/usr/bin/env node
;;   // vim:set ft=javascript:
;;   (function(){
;;       "use strict";
;;        ....

;; put into your own .emacs file (init.el)

;;   (enable-vim-filetype)

;; `vim-filetype-mode-alist' have dummy filename that is delegate of major-mode.

;;; Code:

(require 's)
(require 'dash)

(defcustom vim-filetype-line-re
  "vim: *set +\\(?:ft\\|filetype\\)=\\(.+\\):"
  "Regexp of Vim filetype line."
  :group 'vim-filetype
  :type  'regexp)

(defcustom vim-filetype-mode-alist
  '((basic        . ("/dir/file.bas"))
    (c            . ("/dir/file.c"))
    (changelog    . ("/dir/Changelog"))
    (clojure      . ("/dir/file.clj"))
    (cpp          . ("/dir/file.cpp"))
    (cs           . ("/dir/file.cs"))
    (csh          . (sh-mode . (lambda () (sh-set-shell "csh"))))
    (css          . ("/dir/file.css"))
    (debchangelog . ("/dir/debian/changelog"))
    (debcontrol   . ("/dir/debian/control"))
    (debsources   . ("/dir/sources.list"))
    (dosbatch     . ("/dir/file.bat"))
    (dosini       . ("/dir/file.ini"))
    (erlang       . ("/dir/file.erl"))
    (esqlc        . (sql-mode . (lambda () (sql-set-product 'informix))))
    (freebasic    . ("/dir/file.fb"))
    (gdb          . (gdb-script-mode))
    (go           . ("/dir/file.go"))
    (haml         . ("/dir/file.haml"))
    (haskell      . ("/dir/file.hs"))
    (html         . ("/dir/file.html"))
    (java         . ("/dir/file.java"))
    (javascript   . ("/dir/file.js"))
    (json         . ("/dir/file.json"))
    (m4           . ("/dir/file.m4"))
    (markdown     . ("/dir/file.md"))
    (msql         . (sql-mode))
    (mysql        . (sql-mode . (lambda () (sql-set-product 'mysql))))
    (pascal       . ("/dir/file.pas"))
    (perl         . ("/dir/file.pl"))
    (perl6        . ("/dir/file.p6"))
    (plsql        . (sql-mode . (lambda () (sql-set-product 'oracle))))
    (python       . ("/dir/file.py"))
    (rst          . ("/dir/file.rst"))
    (ruby         . ("/dir/file.rb"))
    (sass         . ("/dir/file.sass"))
    (scheme       . ("/dir/file.scm"))
    (scss         . ("/dir/file.scss"))
    (sql          . (sql-mode))
    (sqlinformix  . (sql-mode . (lambda () (sql-set-product 'informix))))
    (sqloracle    . (sql-mode . (lambda () (sql-set-product 'oracle))))
    (tcsh         . (sh-mode . (lambda () (sh-set-shell "tcsh"))))
    (texinfo      . ("/dir/file.texi"))
    (vb           . ("/dir/file.vb"))
    (vim          . ("/dir/file.vim"))
    (xhtml        . ("/dir/file.xhtml"))
    (xml          . ("/dir/file.xml"))
    (yaml         . ("/dir/file.yml"))
    (zsh          . (sh-mode . (lambda () (sh-set-shell "zsh")))))
  "Alist of Vim-filetype vs dummy filename."
  :group 'vim-filetype
  :type  '(alist :key-type symbol :value-type list))

;;;###autoload
(defun vim-filetype-magic-mode (&optional ft)
  "Invoke `major-mode' by Vim-style `FT' file header."
  (interactive)
  (let* ((bufs (buffer-substring-no-properties (point-min) (point-max)))
         (lang (or ft (cadr (s-match vim-filetype-line-re bufs)))))
    (when lang
      (let* ((data (cdr (assq (intern lang) vim-filetype-mode-alist)))
             (file (car data))
             (vim-major-mode
              (if (symbolp file) file
                (assoc-default file auto-mode-alist #'string-match))))
        (when vim-major-mode
          (funcall vim-major-mode)
          (when (cdr data)
            (funcall (cdr data)))
          vim-major-mode)))))

;;;###autoload
(defun enable-vim-filetype ()
  "Turn on magic-mode by Vim-style file header."
  (interactive)
  (add-to-list 'magic-mode-alist '(vim-filetype-magic-mode . vim-filetype-magic-mode)))

(provide 'vim-filetype)
;;; vim-filetype.el ends here
