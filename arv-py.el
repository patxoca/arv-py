;;; arv-py.el --- helpers for python programming

;; $Id:$

;; Emacs List Archive Entry
;; Filename: arv-py.el
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


;;; Code:

(defcustom pyx/electric-colon-enabled t
  "Non-nil enables `pyx/electric-colon' electric behaviour.
Setting this variable to t is not enough to make : electric, the
keybinding must be redefined."
  :group 'arv-py
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


(provide 'arv-py)

;;; arv-py.el ends here
