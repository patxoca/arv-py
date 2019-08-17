;;; test-helper.el --- Helpers for arv-py-test.el

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

(defun -insert-and-goto-mark (text point-mark)
  (save-excursion
    (insert text))
  (search-forward point-mark)
  (delete-char (- (length point-mark))))

(defun -insert-and-select-region (text open-mark close-mark)
  (save-excursion
    (insert text))
  (transient-mark-mode 1)
  (search-forward open-mark)
  (delete-char (- (length open-mark)))
  (set-mark-command nil)
  (search-forward close-mark)
  (delete-char (- (length close-mark))))

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

;;; test-helper.el ends here
