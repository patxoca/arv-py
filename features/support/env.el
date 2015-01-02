(require 'f)

(defvar arv-py-support-path
  (f-dirname load-file-name))

(defvar arv-py-features-path
  (f-parent arv-py-support-path))

(defvar arv-py-root-path
  (f-parent arv-py-features-path))

(add-to-list 'load-path arv-py-root-path)

(require 'arv-py)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
