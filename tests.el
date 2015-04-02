;;; tests.el --- tests for tests

(require 'ert)
(require 'pyx)


;;; helper stuff

(defmacro with-electric-colon-enabled (&rest body)
  `(let ((pyx/electric-colon-enabled t))
     ,@body))

(defmacro with-electric-colon-disabled (&rest body)
  `(let ((pyx/electric-colon-enabled nil))
     ,@body))

(defmacro with-python-buffer (&rest body)
  `(with-temp-buffer
     (let ((python-indent-guess-indent-offset nil)
           (python-indent-offset 4))
       (python-mode)
       ,@body)))

(defun -get-line (n)
  (save-excursion
    (goto-line n)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun -should-insert-newline-and-indent (text)
  (with-python-buffer
    (let ((current-line -1))
      (insert text)
      (setq current-line (line-number-at-pos))
      (call-interactively 'pyx/electric-colon)
      (should (= (line-number-at-pos) (+ current-line 1)))
      (should (= (current-column) 4))
      (should (string-equal (-get-line 1) (concat text ":"))))))

(defun -should-not-insert-newline-nor-indent (text)
  (with-python-buffer
    (let ((current-line -1))
      (insert text)
      (setq current-line (line-number-at-pos))
      (call-interactively 'pyx/electric-colon)
      (should (= (line-number-at-pos) current-line))
      (should (= (current-column) (+ (length text) 1)))
      (should (string-equal (-get-line 1) (concat text ":"))))))

(defun -display-buffer ()
  (message "**********")
  (message (buffer-substring-no-properties (point-min) (point-max)))
  (message "**********"))

;;; actual tests

;; electric-colon

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


(ert-deftest pyx/test-indentation-respects-return ()
  ""
  (with-electric-colon-enabled
   (with-python-buffer
    (insert "def foo():\n")
    (insert "    return bla\n")
    (insert "def bar()")
    (call-interactively 'pyx/electric-colon)
    (forward-line -1)
    (should (looking-at "def bar"))
    )))

(ert-deftest pyx/test-indentation-bug ()
  ""
  (with-electric-colon-enabled
   (with-python-buffer
    (insert "try\n")
    (insert "    a = 1\n")
    (insert "    except ValueError")
    (call-interactively 'pyx/electric-colon)
    (forward-line -1)
    (should (looking-at "except ValueError"))
    )))

(ert-deftest pyx/test-ec-not-reindents-at-block-start ()
  ""
  (with-electric-colon-enabled
   (with-python-buffer
    (insert "for i in range(3):\n")
    (insert "    print i\n")
    (insert "for i in range(4)\n")
    (call-interactively 'pyx/electric-colon)
    (forward-line -1)
    (should (looking-at "for i in range(4)")))))

;; refactoring

(ert-deftest pyx/test-refactory-wrap-region-no-closing ()
  ""
  (with-python-buffer
   (insert "first sentence\n")
   (insert "second sentence\n")
   (insert "third sentence\n")
   (pyx/-refactor-wrap-region 1 20 "if:")
   (goto-char (point-min))
   (should (looking-at-p "\
if:
    first sentence
    second sentence
third sentence"))))

(ert-deftest pyx/test-refactory-wrap-region-single-line-closing ()
  ""
  (with-python-buffer
   (insert "first sentence\n")
   (insert "second sentence\n")
   (insert "third sentence\n")
   (pyx/-refactor-wrap-region 1 20 "if:" "else:")
   (goto-char (point-min))
   (should (looking-at-p "\
if:
    first sentence
    second sentence
else:
third sentence"))))

(ert-deftest pyx/test-refactory-wrap-region-multi-line-closing ()
  ""
  (with-python-buffer
   (insert "first sentence\n")
   (insert "second sentence\n")
   (insert "third sentence\n")
   (pyx/-refactor-wrap-region 1 20 "if:" "else:\n    pass")
   (goto-char (point-min))
   (should (looking-at-p "\
if:
    first sentence
    second sentence
else:
    pass
third sentence"))))

(ert-deftest pyx/test-refactory-wrap-region-check-point-position-when-point-marker-in-opening ()
  ""
  (with-python-buffer
   (insert "first sentence\n")
   (insert "second sentence\n")
   (insert "third sentence\n")
   (pyx/-refactor-wrap-region 1 20 "if $0:" "else:")
   (insert "Foo")
   (goto-char (point-min))
   (should (looking-at-p "\
if Foo:
    first sentence
    second sentence
else:
third sentence"))))

(ert-deftest pyx/test-refactory-wrap-region-check-point-position-when-point-marker-in-closing ()
  ""
  (with-python-buffer
   (insert "first sentence\n")
   (insert "second sentence\n")
   (insert "third sentence\n")
   (pyx/-refactor-wrap-region 1 20 "if:" "else:\n    $0")
   (insert "Foo")
   (goto-char (point-min))
   (should (looking-at-p "\
if:
    first sentence
    second sentence
else:
    Foo
third sentence"))))

(ert-deftest pyx/test-refactory-wrap-region-check-point-position-when-no-point-marker ()
  ""
  (with-python-buffer
   (insert "first sentence\n")
   (insert "second sentence\n")
   (insert "third sentence\n")
   (pyx/-refactor-wrap-region 1 20 "if:" "else:")
   (insert "Foo")
   (goto-char (point-min))
   (should (looking-at-p "\
if:
    first sentence
    second sentence
else:
Foothird sentence"))))

(ert-deftest test-bug-wraping-indented-region-does-not-indents-continuation-block ()
  ""
  (with-python-buffer
   (insert "def run_task():\n")
   (insert "    task = 1234")
   (pyx/-refactor-wrap-region 20 25 "try:" "except:\n    pass")
   (goto-char (point-min))
   (should (looking-at-p "\
def run_task():
    try:
        task = 1234
    except:
        pass"))))


;;;  tests.el ends here
