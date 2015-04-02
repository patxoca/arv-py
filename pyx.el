;;; pyx.el --- helpers for python programming

;; $Id:$

;; Emacs List Archive Entry
;; Filename: pyx.el
;; Version: $Revision:$
;; Keywords:
;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Created: 2015-01-02
;; Description: helpers for python programming
;; URL: https://github.com/patxoca/arv-py
;; Compatibility: Emacs24

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Extensions for python-mode (the one bundled with emacs).
;;
;; Entry points:
;;
;; - pyx/electric-colon

;;; Dependencies:
;;

;;; History:
;;


(require 'cl-lib)
(require 's)

;;; Code:

(defgroup pyx nil
  "Python extensions."
  :group 'python
  :version "24.3")


(defcustom pyx/electric-colon-enabled t
  "Non-nil enables `pyx/electric-colon' electric behaviour.
Setting this variable to t is not enough to make : electric, the
keybinding must be redefined."
  :group 'pyx
  :type  'boolean
  :safe  'booleanp)


;;;###autoload
(defun pyx/electric-colon (arg)
  "Inserts a newline after :.
This function extends `python-indent-electric-colon' inserting a
newline and indenting that new line. It tries to be smart
regarding when to and when not to insert that newline."
  (interactive "*P")
  (let ((bol (line-beginning-position)))
    (if (and pyx/electric-colon-enabled
             (eolp)
             (not (python-syntax-comment-or-string-p))
             (save-excursion
               (beginning-of-line)
               (save-match-data
                 (looking-at "^\s*\\(class\\|def\\|if\\|elif\\|else\\|for\\|while\\|try\\|except\\|finally\\|with\\)\\b")))
             (not (looking-back "\\[[^]]*" bol))
             (not (looking-back "{[^}]*" bol))
             (not (looking-back "lambda.*" bol)))
        (progn
          (if (fboundp 'python-indent-electric-colon)
              (python-indent-electric-colon arg)
            (insert ":")
            (save-excursion
              (beginning-of-line)
              (when (looking-at "^\s*\\(elif\\|else\\|except\\|finally\\)\\b")
                ;; HACK: aparently indent-for-tab-command behaves as
                ;; expected only at BOL
                (back-to-indentation)
                (indent-for-tab-command))))
          (newline-and-indent))
      (insert ":"))))


;;; simple refactoring

(defun pyx/-get-expanded-region-beginning (start)
  (save-excursion
    (goto-char start)
    (line-beginning-position)))


(defun pyx/-get-expanded-region-end (end)
  (save-excursion
    (goto-char end)
    (forward-line)
    (point)))

(defun pyx/-insert-and-indent-rigidly (text indentation-level)
  (when text
    (let ((indentation (make-string indentation-level ?\s)))
      (cl-dolist (line (s-split "\n" text))
        (insert indentation)
        (insert line)
        (newline)))))

(defun pyx/-refactor-wrap-region (begin end opening &optional closing point-mark)
  "Wraps all lines intersecting the region BEGIN END within an
OPENING and optional CLOSING block. Moves point to the position
given by POINT-MARK."
  (let ((point-mark (or point-mark "$0"))
        (begin (pyx/-get-expanded-region-beginning begin))
        (end (pyx/-get-expanded-region-end end))
        (end-marker (copy-marker (pyx/-get-expanded-region-end end) t))
        (indentation (save-excursion
                       (goto-char begin)
                       (back-to-indentation)
                       (current-column))))
    (when closing
        (goto-char end)
        (if (eobp)
            (newline))
        (pyx/-insert-and-indent-rigidly closing indentation))
    (indent-rigidly begin end python-indent-offset)
    (goto-char begin)
    (pyx/-insert-and-indent-rigidly opening indentation)
    (save-restriction
      (narrow-to-region begin end-marker)
      (goto-char (point-min))
      (if (search-forward point-mark nil t)
          (delete-char (- (length point-mark)))
        (goto-char (point-max))))))

(defun pyx/refactor-wrap-if-else (begin end)
  "Wrap all lines intersecting the region within an 'if/else'
statement."
  (interactive "r")
  (pyx/-refactor-wrap-region begin end "if $0:" "else:\n    pass"))

(defun pyx/refactor-wrap-try-except (begin end)
  "Wrap all lines intersecting the region within an 'try/except'
statement."
  (interactive "r")
  (pyx/-refactor-wrap-region begin end "try:" "except $0 as e:\n    pass"))

(defun pyx/refactor-wrap-while (begin end)
  "Wrap all lines intersecting the region within an 'while'
loop."
  (interactive "r")
  (pyx/-refactor-wrap-region begin end "while $0:"))

(defun pyx/refactor-wrap-for (begin end)
  "Wrap all lines intersecting the region within an 'foo'
loop."
  (interactive "r")
  (pyx/-refactor-wrap-region begin end "while $0:"))


;;; assorted utilities

(defun pyx/get-current-package-name ()
  "Return the current package name.

The package name is the name of the directory that contains the
'setup.py'. If no 'setup.py' is found nil is returned."
  (let ((parent (locate-dominating-file (buffer-file-name) "setup.py")))
    (if (null parent)
        nil
      (file-name-base (directory-file-name parent)))))

(defun pyx/get-current-module-fqdn ()
  "Return the current modules fully qualified dotted name.

TODO: assumes '/' path separator. Not tested on windows.
"
  (let ((package-root (locate-dominating-file (buffer-file-name) "setup.py"))
        (module-abs-path (if (s-ends-with? "/__init__.py" (buffer-file-name))
                             (s-chop-suffix "/__init__.py" (buffer-file-name))
                           (file-name-sans-extension (buffer-file-name)))))
    (when package-root
      (replace-regexp-in-string
       "/" "."
       (substring module-abs-path (length (expand-file-name package-root)))))))

(defun pyx/electric-grave ()
  "Inserts ```` within a string or just ` elsewhere."
  (interactive)
  (if (nth 8 (syntax-ppss))
      (progn
        (insert "````")
        (backward-char 2))
    (insert "`")))

(provide 'pyx)

;;; pyx.el ends here
