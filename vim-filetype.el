;;; vim-filetype.el --- Parse Vim-style filetype header

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

;; 
;; vim:set ft=lisp:

;; put into your own .emacs file (init.el)

;;   (enable-vim-filetype)


;;; Code:

(require 's)
(require 'dash)

(defcustom vim-filetype-line-re
  "vim: *set +\\(?:ft\\|filetype\\)=\\(.+\\):"
  "Regexp of Vim filetype line."
  :group 'vim-filetype
  :type  'regexp)

(defcustom vim-filetype-mode-alist
  '(("c"            . ("/dir/file.c"))
    ("changelog"    . ("/dir/Changelog"))
    ("clojure"      . ("/dir/file.clj"))
    ("cs"           . ("/dir/file.cs"))
    ("css"          . ("/dir/file.css"))
    ("debchangelog" . ("/dir/debian/changelog"))
    ("debcontrol"   . ("/dir/debian/control"))
    ("debsources"   . ("/dir/sources.list"))
    ("esqlc"        . ("/dir/file.sql" (sql-product . informix)))
    ("gdb"          . ('gdb-script-mode))
    ("go"           . ("/dir/file.go"))
    ("haml"         . ("/dir/file.haml"))
    ("haskell"      . ("/dir/file.hs"))
    ("html"         . ("/dir/file.html"))
    ("java"         . ("/dir/file.java"))
    ("javascript"   . ("/dir/file.js"))
    ("json"         . ("/dir/file.json"))
    ("markdown"     . ("/dir/file.md"))
    ("msql"         . ("/dir/file.sql" (sql-product . mysql)))
    ("pascal"       . ("/dir/file.pas"))
    ("perl"         . ("/dir/file.pl"))
    ("plsql"        . ("/dir/file.sql" (sql-product . oracle)))
    ("python"       . ("/dir/file.py"))
    ("ruby"         . ("/dir/file.rb"))
    ("sass"         . ("/dir/file.sass"))
    ("scheme"       . ("/dir/file.scm"))
    ("sql"          . ("/dir/file.sql"))
    ("sqlinformix"  . ("/dir/file.sql" (sql-product . informix)))
    ("sqloracle"    . ("/dir/file.sql" (sql-product . oracle)))
    ("vim"          . ("/dir/file.vim"))
    ("xhtml"        . ("/dir/file.xhtml"))
    ("xml"          . ("/dir/file.xml"))
    ("zsh"          . ("/dir/file.sh")))
  ""
  :group 'vim-filetype
  :type  '(alist :key-type string :value-type list))

;;;###autoload
(defun vim-filetype-magic-mode ()
  ""
  (interactive)
  (let* ((bufs (buffer-substring-no-properties (point-min) (point-max)))
         (lang (cadr (s-match vim-filetype-line-re bufs))))
    (when lang
      (let* ((data (assoc-default lang vim-filetype-mode-alist))
             (file (car data))
             (vim-major-mode
              (if (symbolp file) (eval file)
                (assoc-default file auto-mode-alist #'string-match))))
        (when vim-major-mode
          (funcall vim-major-mode)
          data
          (--each (cdr data)
            (set (car it) (cdr it)))
          vim-major-mode)))))

;;;###autoload
(defun enable-vim-filetype ()
  ""
  (interactive)
  (add-to-list 'magic-mode-alist `(,vim-filetype-line-re . vim-filetype-magic-mode)))

(provide 'vim-filetype)
;;; vim-filetype.el ends here
