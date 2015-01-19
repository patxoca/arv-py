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
  (if (fboundp 'python-indent-electric-colon)
      (python-indent-electric-colon arg)
    (insert ":")
    (indent-for-tab-command))
  (when pyx/electric-colon-enabled
    (let ((bol (line-beginning-position)))
      (if (and
           (eolp)
           (not (python-syntax-comment-or-string-p))
           (save-excursion
             (beginning-of-line)
             (save-match-data
               (looking-at "^\s*\\(class\\|def\\|if\\|elif\\|else\\|for\\|while\\|try\\|except\\|finally\\|with\\)\\b")))
           (not (looking-back "\\[[^]]*" bol))
           (not (looking-back "{[^}]*" bol))
           (not (looking-back "lambda.*" bol)))
          (newline-and-indent)))))


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

(defun pyx/-refactor-wrap-region (begin end opening &optional closing point-mark)
  "Wraps all lines intersecting the region BEGIN END within an
OPENING and optional CLOSING block. Moves point to the position
given by POINT-MARK."
  (let ((point-mark (or point-mark "$0"))
        (begin (pyx/-get-expanded-region-beginning begin))
        (indentation (save-excursion
                       (goto-char begin)
                       (back-to-indentation)
                       (current-column)))
        (start-marker (copy-marker (pyx/-get-expanded-region-beginning begin) nil))
        (stop-marker (copy-marker (pyx/-get-expanded-region-end end) t)))
    (when closing
        (goto-char stop-marker)
        (if (eobp)
            (newline))
        (cl-dotimes (i indentation)
          (insert " "))
        (cl-dolist (line (s-split "\n" closing))
          (insert line)
          (indent-according-to-mode)
          (newline)))
    (indent-rigidly begin end python-indent-offset)
    (goto-char start-marker)
    (cl-dotimes (i indentation)
      (insert " "))
    (insert opening)
    (newline)
    (save-restriction
      (narrow-to-region start-marker stop-marker)
      (goto-char (point-min))
      (if (search-forward point-mark nil t)
          (delete-char (- (length point-mark)))
        (goto-char (point-max))))))

(defun pyx/refactor-wrap-if-else (begin end)
  "Wrap all lines intersecting the regions within an 'if/else'
statement."
  (interactive "r")
  (pyx/-refactor-wrap-region begin end "if $0:" "else:\npass"))

(defun pyx/refactor-wrap-try-except (begin end)
  "Wrap all lines intersecting the regions within an 'try/except'
statement."
  (interactive "r")
  (pyx/-refactor-wrap-region begin end "try:" "except $0 as e:\npass"))


(provide 'pyx)

;;; pyx.el ends here
