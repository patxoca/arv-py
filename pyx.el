;;; pyx.el --- helpers for python programming

;; $Id:$

;; Emacs List Archive Entry
;; Filename: pyx.el
;; Version: 0.1
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
(require 'dash)
(require 'f)
(require 's)

;;; Code:

(defgroup pyx nil
  "Python extensions."
  :group 'python
  :version "24.3")


;;; Private helpers

(defun pyx/--is-toplevel-object ()
  "Determina si es tracta d'una definició de nivell superior.

Avalua a t si el punt es troba en una línia que conté una
element de nivell més alt.  Esta pensada per ser cridada al
començament de blocs (if, def, class ...).  En altes contexts pot
compotar-se de manera estranya."
  (save-excursion
    (let ((start (progn
		   (beginning-of-line)
		   (point)))
	  (indent (progn
		    (back-to-indentation)
		    (point))))
      (= start indent))))

(defun pyx/--info-looking-at-beginning-of-class ()
  "Determina si es tracta de la definició d'una classe.

Avalua a t si el punt es troba en una línia que conté la paraula
reservada `class' en el context de la definició d'una classe."
  (and
   (python-info-looking-at-beginning-of-defun)
   (save-excursion
     (back-to-indentation)
     (looking-at "class[[:space:]]+"))))

(defun pyx/--info-looking-at-beginning-of-def ()
  "Determina si es tracta de la definició d'una funció/mètode.

Avalua a t si el punt es troba en una línia que conté la paraula
reservada `def' en el context de la definició d'una
funció/mètode."
  (and
   (python-info-looking-at-beginning-of-defun)
   (save-excursion
     (back-to-indentation)
     (looking-at "def[[:space:]]+"))))


;;; Navigation

(defun pyx/nav-beginning-of-class ()
  "Mou el punt al principi de la classe."
  (while (and (not (pyx/--info-looking-at-beginning-of-class))
	      (not (pyx/--is-toplevel-object)))
    (python-nav-beginning-of-defun 1)))

(defun pyx/nav-beginning-of-def ()
  "Mou el punt al principi de la funció/mètode."
  (while (and (not (pyx/--info-looking-at-beginning-of-def))
	      (not (pyx/--is-toplevel-object)))
    (python-nav-beginning-of-defun 1)))

;;;###autoload
(defun pyx/nav-goto-first-import ()
  "Mou el punt al començament del primer 'import' guardant el
punt anterior en el `mark-ring'. Si no troba cap import deixa el
punt al mateix lloc.

Retorna la posició del punt si s'ha trobat cap 'import' o `nil'
en cas contrari."
  (interactive)
  (let ((case-fold-search nil)
        (matched nil))
    (save-excursion
      (goto-char (point-min))
      (while (and
              (setq matched (search-forward-regexp "\\<import\\>" nil t))
              (python-syntax-comment-or-string-p))))
    (when matched
      (push-mark (point) t nil)
      (goto-char matched)
      (beginning-of-line))))


;;; Query program structure

(defun pyx/info-enclosing-def-name ()
  "Determina el nom de la funció dins la que es troba el punt.

Retorna una cadena amb el nom de la funció més interna dins al
que es troba el punt o nil si el punt no es troba dins de cap
funció."
  (let ((result nil))
    (save-excursion
      (pyx/nav-beginning-of-def)
      (back-to-indentation)
      (if (looking-at "def +\\([_[:alnum:]]+\\)")
        (setq result (match-string 1))))
    result))

(defun pyx/info-enclosing-class-name ()
  "Determina el nom de la classe dins la que es troba el punt.

Retorna una cadena amb el nom de la classe més interna dins la
que es troba el punt o nil si el punt no es troba dins de cap
classe."
  (let ((result nil))
    (save-excursion
      (pyx/nav-beginning-of-class)
      (back-to-indentation)
      (if (looking-at "class +\\([_[:alnum:]]+\\)" )
        (setq result (match-string 1))))
    result))


;;; outline-mode

;; (defun arv-py-outline-level ()
;;   "Retorna el outline-level corresponent a elements d'un programa
;; python. És una prova de concepte per jugar amb outline (que te
;; certes limitacions.)"
;;   (interactive)
;;   (let ((level (- (match-end 0) (match-beginning 0) (length (match-string 1)))))
;;     (if (member (match-string 1) python-indent-dedenters)
;;         (+ level (/ python-indent-offset 2))
;;       level)))


;;;###autoload
(defun pyx/visit-setup-py ()
  ""
  (interactive)
  (let ((parent (locate-dominating-file (buffer-file-name) "setup.py")))
    (if (not parent)
        (message "'setup.py' not found")
      (find-file (concat (file-name-as-directory parent) "setup.py")))))

;;;###autoload
(defun pyx/visit-test-module ()
  "Visit the test module for the current module.

In order to find the test module it looks for a module called
'tests.py' both in the distribution and package root directories
and for a module called 'test_MODULE.py' in the subdirectory
'tests' of the ditribution and package root directories."
  (interactive)
  (when (buffer-file-name)
    (let* ((module-file-name (file-name-nondirectory (buffer-file-name)))
           (package-root (pyx/get-distribution-root))
           (package-src-root (pyx/get-package-root)))
      (when (and package-root package-src-root)
        (let ((result (-first #'file-exists-p
                              (list (f-join package-root "tests.py")
                                    (f-join package-src-root "tests.py")
                                    (f-join package-root
                                            "tests"
                                            (concat "test_" module-file-name))
                                    (f-join package-src-root
                                            "tests"
                                            (concat "test_" module-file-name))))))
          (when result
            (find-file result)))))))

;;;###autoload
(defun pyx/insert-current-package-name ()
  "Insert the current package name.

The package name is the name of the directory that contains the
'setup.py'."
  (interactive)
  (let ((name (pyx/get-current-package-name)))
    (if (null name)
        (error "'setup.py' not found")
      (insert name))))


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
  "Return point at the beginning of the line containg the
position START (number-or-marker-p)."
  (save-excursion
    (goto-char start)
    (line-beginning-position)))


(defun pyx/-get-expanded-region-end (end)
  "Return point at the beginning of the first line after position
END. If END is in the last line inserts a new line."
  (save-excursion
    (goto-char end)
    (forward-line)
    (if (eobp)
        (newline))
    (point)))

(defun pyx/-insert-and-indent (text indentation-level)
  (when text
    (let ((indentation (make-string indentation-level ?\s)))
      (cl-dolist (line (s-split "\n" text))
        (insert indentation)
        (if (not (s-starts-with-p ">" line))
            (insert line)
          (insert (substring line 1))
          (indent-according-to-mode))
        (newline)))))

(defun pyx/-refactor-wrap-region (begin end opening &optional closing point-mark)
  "Wraps all lines intersecting the region BEGIN END within an
OPENING and optional CLOSING block. Moves point to the position
given by the mark POINT-MARK, $0 by default.

Both OPENING and CLOSING may be multiline. Lines starting with
'>' are indented according to the mode.
"
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
        (pyx/-insert-and-indent closing indentation))
    (indent-rigidly begin end python-indent-offset)
    (goto-char begin)
    (pyx/-insert-and-indent opening indentation)
    ;; leave point at POINT-MARK and delete the mark
    (save-restriction
      (narrow-to-region begin end-marker)
      (goto-char (point-min))
      (if (search-forward point-mark nil t)
          (delete-char (- (length point-mark)))
        (goto-char (point-max))))))

;;;###autoload
(defun pyx/refactor-wrap-if-else (begin end)
  "Wrap all lines intersecting the region within an 'if/else'
statement."
  (interactive "r")
  (pyx/-refactor-wrap-region begin end "if $0:" "else:\n>pass"))

;;;###autoload
(defun pyx/refactor-wrap-try-except (begin end)
  "Wrap all lines intersecting the region within an 'try/except'
statement."
  (interactive "r")
  (pyx/-refactor-wrap-region begin end "try:" "except $0 as e:\n>pass"))

;;;###autoload
(defun pyx/refactor-wrap-while (begin end)
  "Wrap all lines intersecting the region within an 'while'
loop."
  (interactive "r")
  (pyx/-refactor-wrap-region begin end "while $0:"))

;;;###autoload
(defun pyx/refactor-wrap-for (begin end)
  "Wrap all lines intersecting the region within an 'for'
loop."
  (interactive "r")
  (pyx/-refactor-wrap-region begin end "for $0:"))

;;;###autoload
(defun pyx/refactor-wrap-with (begin end)
  "Wrap all lines intersecting the region within an 'with'
statement."
  (interactive "r")
  (pyx/-refactor-wrap-region begin end "with $0:"))


;;; assorted utilities

(defun pyx/get-current-package-name ()
  "Return the current package name.

The package name is the name of the directory that contains the
'setup.py'. If no 'setup.py' is found nil is returned."
  (let ((parent (locate-dominating-file (buffer-file-name) "setup.py")))
    (if (null parent)
        nil
      (file-name-base (directory-file-name parent)))))

(defun pyx/get-distribution-root ()
  "Return the package's distribution root.

The distribution root is the directory containing the 'setup.py'
file."
  (let ((root (locate-dominating-file (buffer-file-name) "setup.py")))
    (when root
      (expand-file-name root))))

(defun pyx/get-package-root ()
  "Return the package root.

The package root is the directory containing the actual source
code. It's a direct child of the distribution root. It's assumed
that the function is called from a buffer inside the src tree."
  (let ((pkg-root (pyx/get-distribution-root))
        (foo (buffer-file-name)))
    (when (and pkg-root foo)
      (let ((bar (f-split foo)))
        (apply #'f-join (subseq bar 0 (1+ (length (f-split pkg-root)))))))))


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

;;;###autoload
(defun pyx/smart-grave ()
  "Tries to be smart about ` usage patterns.

Within a string it assumes that we are writing rst, after :
inserts `` (like in :xref:`whatever`) otherwise insert
```` (inline code literal). In both cases point is left in the
middle.

Elsewhere inserts a single `."
  (interactive)
  (if (nth 8 (syntax-ppss))
      (let ((begin (point))
            (end   (point))
            (active (region-active-p))
            (delimiter))
        (when active
          (setq begin (min (region-beginning) (region-end)))
          (setq end   (max (region-beginning) (region-end))))
        (goto-char begin)
        (setq delimiter (if (looking-back ":\\w+:" (line-beginning-position))
                            "`"
                          "``"))
        (insert delimiter)
        (goto-char (+ end (length delimiter)))
        (insert delimiter)
        (unless active
          (backward-char (length delimiter))))
    (insert "`")))

;;;###autoload
(defun pyx/make ()
  "Runs make.

Search for a Makefile in parent directories. If found prompt the
user for the target (with completion) and run it."
  (interactive)
  (let ((dir (locate-dominating-file (buffer-file-name) "Makefile")))
    (when dir
      (let* ((target-string (shell-command-to-string (format "cd %s && make -prn | sed -rn '/^[^# \\t\\.%%].*:[^=]?/p' | grep -E '^\\w+:' | cut -d: -f1 | sort" dir)))
             (target-list (cl-remove-if (lambda (x) (string-equal x "Makefile"))
                                        (s-split "\n" target-string t)))
             (target (completing-read "Target: " target-list nil t)))
        (compile (format "cd %s && make %s" dir target) t)))))

;;;###autoload
(defun pyx/add-setup-dependency (module-name)
  "Adds dependency to 'setup.py'.

Search for setup.py in parent directories. If found prompt the
user for a module name and insert it after the '# -*- Extra
requirements: -*-' marker.
"
  (interactive "MModul: ")
  (let ((dir (locate-dominating-file (buffer-file-name) "setup.py")))
    (when dir
      (with-current-buffer (find-file (concat (file-name-as-directory dir) "setup.py"))
        (save-excursion
          (goto-char (point-min))
          (when (search-forward "-*- Extra requirements: -*-" nil t)
            (end-of-line)
            (newline)
            (insert "\"" module-name "\",")
            (indent-according-to-mode)))))))

(provide 'pyx)

;;; pyx.el ends here
