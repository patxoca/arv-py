;;; tests.el --- tests for tests

(require 'ert)
(require 'arv-py)


;;; helper stuff

(defmacro with-electric-colon-enabled (&rest body)
  `(let ((pyx/electric-colon-enabled t))
     ,@body))

(defmacro with-electric-colon-disabled (&rest body)
  `(let ((pyx/electric-colon-enabled nil))
     ,@body))


(defun -get-line (n)
  (save-excursion
    (goto-line n)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun -should-insert-newline-and-indent (text)
  (with-temp-buffer
    (let ((current-line -1)
          (python-indent-guess-indent-offset nil))
      (python-mode)
      (insert text)
      (setq current-line (line-number-at-pos))
      (call-interactively 'pyx/electric-colon)
      (should (= (line-number-at-pos) (+ current-line 1)))
      (should (= (current-column) 4))
      (should (string-equal (-get-line 1) (concat text ":"))))))

(defun -should-not-insert-newline-nor-indent (text)
  (with-temp-buffer
    (let ((current-line -1)
          (python-indent-guess-indent-offset nil))
      (python-mode)
      (insert text)
      (setq current-line (line-number-at-pos))
      (call-interactively 'pyx/electric-colon)
      (should (= (line-number-at-pos) current-line))
      (should (= (current-column) (+ (length text) 1)))
      (should (string-equal (-get-line 1) (concat text ":"))))))


;;; actual tests

(ert-deftest pyx/test-colon-is-electric-when-starting-block ()
  ": is electric when we are starting a block of code"
  (let ((sentences '("def some_function()"
                     "def some_function(param)"
                     "class SomeClass"
                     "class SomeClass(SubClass)"
                     "if foo == bar"
                     "elif bar > baz"
                     "else"
                     "for i in range(10)"
                     "while i < 2"
                     "try"
                     "except"
                     "except ValueError"
                     "except ValueError as e"
                     "finally"
                     "with open('foo.txt') as f")))
    (with-electric-colon-enabled
     (dolist (s sentences)
       (-should-insert-newline-and-indent s)))))

(ert-deftest pyx/test-colon-is-electric-with-multiline-sentences ()
  ": currently multiline statements are not supported"
  :expected-result :failed
  (with-electric-colon-enabled
   (-should-insert-newline-and-indent "def some_function(
param1)")))


(ert-deftest pyx/test-colon-is-not-electric-if-disabled ()
  ": is not electric if disabled in customization"
  (with-electric-colon-disabled
    (-should-not-insert-newline-nor-indent "def some_function()")))

(ert-deftest pyx/test-colon-is-not-electric-within-comments ()
  ": is not electric within comments"
  (with-electric-colon-enabled
   (-should-not-insert-newline-nor-indent "# this is a comment")))

(ert-deftest pyx/test-colon-is-not-electric-within-strings ()
  ": is not electric within strings"
  (with-electric-colon-enabled
   (-should-not-insert-newline-nor-indent "\"double quoted string")
   (-should-not-insert-newline-nor-indent "'single quoted string")))

(ert-deftest pyx/test-colon-is-not-electric-in-dictionaries ()
  ": is not electric after dictionary keys"
  (with-electric-colon-enabled
   (-should-not-insert-newline-nor-indent "var = {'key'")
      (-should-not-insert-newline-nor-indent "'key'")))

(ert-deftest pyx/test-colon-is-not-electric-in-slices ()
  ": is not electric in a list slice"
  (with-electric-colon-enabled
   (-should-not-insert-newline-nor-indent "var = [1")
   (-should-not-insert-newline-nor-indent "[1")))

(ert-deftest pyx/test-colon-is-not-electric-after-lambda ()
  ": is not electric after lambda"
  (with-electric-colon-enabled
   (-should-not-insert-newline-nor-indent "lambda x")))

(ert-deftest pyx/test-colon-is-not-electric-after-keywords-as-prefixes ()
  ": inserts itself"
  (let ((sentences '("classic"
                     "definition"
                     "elifoobar"
                     "elsewhere"
                     "exceptional"
                     "formula"
                     "iffier"
                     "tryout"
                     "whiled"
                     "whithout")))
    (with-electric-colon-enabled
     (dolist (s sentences)
       (-should-not-insert-newline-nor-indent s)))))

(ert-deftest pyx/test-colon-indents-current-line ()
  ""
  (with-electric-colon-enabled
   (with-temp-buffer
     (python-mode)
     (insert "if foo:\n")
     (insert "  v = 1\n")
     (insert "  else")
     (call-interactively 'pyx/electric-colon)
     (forward-line -1)
     (should (looking-at "else")))))



;;;  tests.el ends here
