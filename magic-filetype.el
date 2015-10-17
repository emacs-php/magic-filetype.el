;;; magic-filetype.el --- Parse Vim-style filetype header -*- mode: emacs-lisp; lexical-binding: t -*-
;;; vim: set ft=lisp:

;; Copyright (C) 2015 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 9 Aug 2015
;; Version: 0.0.1
;; Keywords: vim ft file magic-mode
;; Package-Requires: ((emacs "24") (s "1.9.0"))

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

;; `magic-filetype' parse Vim-style file header.
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

(defcustom vim-filetype-line-re
  "vim: *set +\\(?:ft\\|filetype\\)=\\(.+\\):"
  "Regexp of Vim filetype line."
  :group 'magic-filetype
  :type  'regexp)

(defcustom magic-filetype-mode-alist
  '((applescript  . ("/dir/file.scpt"))
    (basic        . ("/dir/file.bas"))
    (c            . ("/dir/file.c"))
    (changelog    . ("/dir/Changelog"))
    (clojure      . ("/dir/file.clj"))
    (coffee       . ("/dir/file.coffee"))
    (commonlisp   . ("/dir/file.lisp"))
    (cpp          . ("/dir/file.cpp"))
    (crystal      . ("/dir/file.cr"))
    (cs           . ("/dir/file.cs"))
    (csh          . (sh-mode . (lambda () (sh-set-shell "csh"))))
    (css          . ("/dir/file.css"))
    (csv          . ("/dir/file.csv"))
    (dart         . ("/dir/file.dart"))
    (debchangelog . ("/dir/debian/changelog"))
    (debcontrol   . ("/dir/debian/control"))
    (debsources   . ("/dir/sources.list"))
    (delphi       . ("/dir/file.pas"))
    (dosbatch     . ("/dir/file.bat"))
    (dosini       . ("/dir/file.ini"))
    (elixir       . ("/dir/file.exs"))
    (emacslisp    . ("/dir/file.el"))
    (erlang       . ("/dir/file.erl"))
    (es6          . ("/dir/file.es6"))
    (esqlc        . (sql-mode . (lambda () (sql-set-product 'informix))))
    (freebasic    . ("/dir/file.fb"))
    (fsharp       . ("/dir/file.fs"))
    (gdb          . (gdb-script-mode))
    (go           . ("/dir/file.go"))
    (haml         . ("/dir/file.haml"))
    (haskell      . ("/dir/file.hs"))
    (html         . ("/dir/file.html"))
    (java         . ("/dir/file.java"))
    (javascript   . ("/dir/file.js"))
    (json         . ("/dir/file.json"))
    (lisp         . ("/dir/file.lisp"))
    (nadeshiko    . ("/dir/file.nako"))
    (m4           . ("/dir/file.m4"))
    (markdown     . ("/dir/file.md"))
    (msql         . (sql-mode))
    (mysql        . (sql-mode . (lambda () (sql-set-product 'mysql))))
    (ocaml        . ("/dir/file.ml"))
    (org          . ("/dir/file.org"))
    (pascal       . ("/dir/file.pas"))
    (perl         . ("/dir/file.pl"))
    (perl6        . ("/dir/file.p6"))
    (plsql        . (sql-mode . (lambda () (sql-set-product 'oracle))))
    (python       . ("/dir/file.py"))
    (rst          . ("/dir/file.rst"))
    (ruby         . ("/dir/file.rb"))
    (sass         . ("/dir/file.sass"))
    (scala        . ("/dir/file.scala"))
    (scheme       . ("/dir/file.scm"))
    (scss         . ("/dir/file.scss"))
    (standardml   . ("/dir/file.sml"))
    (sql          . (sql-mode))
    (sqlinformix  . (sql-mode . (lambda () (sql-set-product 'informix))))
    (sqloracle    . (sql-mode . (lambda () (sql-set-product 'oracle))))
    (swift        . ("/dir/file.swift"))
    (tcsh         . (sh-mode . (lambda () (sh-set-shell "tcsh"))))
    (texinfo      . ("/dir/file.texi"))
    (text         . ("/dir/file.txt"))
    (typescript   . ("/dir/file.ts"))
    (vb           . ("/dir/file.vb"))
    (vim          . ("/dir/file.vim"))
    (xhtml        . ("/dir/file.xhtml"))
    (xml          . ("/dir/file.xml"))
    (yaml         . ("/dir/file.yml"))
    (zsh          . (sh-mode . (lambda () (sh-set-shell "zsh")))))
  "Alist of Vim-filetype vs dummy filename."
  :group 'magic-filetype
  :type  '(alist :key-type symbol :value-type list))

;;;###autoload
(defun major-mode-from-language-name (lang-name)
  "Invoke `major-mode' from `LANG-NAME'."
  (interactive
   (list
    (completing-read "Choose language: " magic-filetype-mode-alist)))
  (when lang-name
    (let* ((data (cdr (assq (intern lang-name) magic-filetype-mode-alist)))
           (file (car data))
           (new-major-mode
            (if (symbolp file) file
              (assoc-default file auto-mode-alist #'string-match))))
      (when new-major-mode
        (funcall new-major-mode)
        (when (cdr data)
          (funcall (cdr data)))
        new-major-mode))))

;;;###autoload
(defun vim-filetype-magic-mode (&optional ft)
  "Invoke `major-mode' by Vim-style `FT' file header."
  (interactive)
  (let* ((bufs (buffer-substring-no-properties (point-min) (point-max)))
         (lang (or ft (cadr (s-match vim-filetype-line-re bufs)))))
    (when lang
      (major-mode-from-language-name lang))))

;;;###autoload
(defun enable-vim-filetype ()
  "Turn on magic-mode by Vim-style file header."
  (interactive)
  (add-to-list 'magic-fallback-mode-alist '(vim-filetype-magic-mode . vim-filetype-magic-mode)))

;;;###autoload
(defun major-mode-of (lang-name)
  "Get MAJOR-MODE from `LANG-NAME'."
  (let* ((data (cdr (assq lang-name magic-filetype-mode-alist)))
         (file (car data))
         (new-major-mode
          (if (symbolp file) file
            (assoc-default file auto-mode-alist #'string-match))))
    (unless new-major-mode (error "Unknown LANG-NAME"))
    (if (cdr data)
        (lambda () (funcall new-major-mode) (funcall (cdr data)))
     new-major-mode)))

;;;###autoload
(defun reload-major-mode ()
  "Reload current major mode."
  (interactive)
  (let ((current-mode major-mode))
    (fundamental-mode)
    (funcall current-mode)
    current-mode))

(provide 'magic-filetype)
;;; magic-filetype.el ends here
