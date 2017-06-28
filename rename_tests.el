;;; rename_tests.el --- Standardize test names in a project
;; 

;;; Code:

(defvar dir "/home/dan/projects/clojure/cider-nrepl")
(defvar clojure-files (directory-files-recursively dir ".clj$"))
(defvar deftest-regexp "^\\s-*(deftest\\s-+")

;;; Commentary:
;; In CIDER-repl, (the java side), test names had gotten unwiedly and
;; no longer conformed to any particular style. This ran over all
;; files looking for test names and checked to see if they conformed,
;; and if not, if they could be easily fixed.

(require 'cl-seq)

(defun valid-test-name-p (test-name)
  "Test name ends with \"-test\"."
  (and (not (string-blank-p test-name))
       (string-suffix-p "-test" test-name)))

(defun pluralized-test-p (test-name)
  (string-suffix-p "tests" test-name))

(defun prefixed-with-test-p (test-name)
  (string-prefix-p "test-" test-name))

(defun lacks-test-p (test-name)
  (not (string-match "test" test-name)))

;; rename to classify
(defun analyze-test-names (name)
  (cond ((valid-test-name-p name) `(,name valid))
        ((pluralized-test-p name) `(,name pluralized))
        ((prefixed-with-test-p name) `(,name prefixed))
        ((lacks-test-p name) `(,name missing-suffix))
        (t `(,name unrecognized))))

(defun fix-testname (name reason)
  (cond ((eq reason 'missing-suffix)
         (format "%s-test" name))
        ((eq reason 'prefixed)
         (fix-testname (replace-regexp-in-string "test-" "" name)
                                             'missing-suffix))
        ((eq reason 'pluralized)
         (substring name 0 (- (length name) 1)))))

(defun deftest-regexp-for (name)
  (concat deftest-regexp (escape-name name)))

(defun appears-once-and-only-once-p (name)
  (let ((occurences 0)
        (regexp (deftest-regexp-for name)))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (incf occurences))
    (= 1 occurences)))


;; data access
;; building up data like (filename ((testname classification) ...))
(defun names-and-states (result)
  (cadr result))

(defun testname-of (name-and-state)
  (car name-and-state))

(defun state-of (name-and-state)
  (cadr name-and-state))

(defun filename-of (result)
  (car result))

(defun can-fix (reason)
  "Determine if REASON can be programatically fixed where REASON
is one of unrecognized, pluralized, prefixed, missing-suffix"
  (member reason '(missing-suffix prefixed pluralized)))

(defun escape-name (name)
  "Annoyingly, one test NAME had a question mark in it, which
must be made a literal question mark in the regex rather than an
optional specifier."
  (replace-regexp-in-string "?" "\\?" name))

(defun make-new-testname! (testpair)
  "Assumes it is inside of a buffer. be careful. TESTPAIR is a
list of testname and classification like (\"data-test\" valid) or
some other combination. We remember the old name, determine the
new name, make sure that we can correctly locate it, the replace
it."
  (let ((old-name (testname-of testpair))
        (reason (state-of testpair)))
    (when (and (can-fix reason)
               (appears-once-and-only-once-p old-name))
      (let ((new-name (fix-testname old-name reason))
            (regexp (deftest-regexp-for old-name)))
        (goto-char (point-min))
        (re-search-forward regexp nil t 1)
        (beginning-of-line)
        (re-search-forward (escape-name old-name) nil t)
        (replace-match new-name)))))

(defun get-test-names (file)
  "Walk though a FILE looking for deftest forms. This grabs the
text after deftest until the end of the line. Could definitely be better"
  (let ((test-names))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward deftest-regexp nil t)
        (push (buffer-substring-no-properties (point) (progn (move-end-of-line 1) (point)))
              test-names)))
    test-names))

(defun process-test-names-for-file (file)
  "Create the data structure of a list of filename with a list of
test names and analysis."
  (let ((testnames (get-test-names file)))
    (when testnames
      (list file (mapcar #'analyze-test-names testnames)))))

(defun fixup-tests-for (files)
  (let ((test-infos (remove-if #'null (mapcar #'process-test-names-for-file files))))
    (mapc #'(lambda (test-info)
              (with-current-buffer (find-file (filename-of test-info))
                (mapc #'make-new-testname! (names-and-states test-info))
                (save-buffer)))
          test-infos)))

(defun mapcon (f l)
  (apply #'append (mapcar f l)))

;; filter the lists of the data to just the interesting classifications
;; (remove-if #'(lambda (l) (not (names-and-states l)))
;;            (mapcar #'(lambda (test-info)
;;                        (list (filename-of test-info)
;;                              (remove-if-not #'(lambda (testpair)
;;                                                 (eq 'prefixed (state-of testpair)))
;;                                             (names-and-states test-info))))
;;                    (remove-if #'null (mapcar #'process-test-names-for-file clojure-files))))
(let ((valid 0)
      (pluralized 0)
      (prefixed 0)
      (missing-suffix 0)
      (unrecognized 0))
  (mapc #'(lambda (tests)
            (mapc #'(lambda (result)
                      (pcase (cadr result)
                        ('valid (incf valid))
                        ('pluralized (incf pluralized))
                        ('prefixed (incf prefixed))
                        ('missing-suffix (incf missing-suffix))
                        ('unrecognized (incf unrecognized))))
                  (cadr tests)))
        (remove-if #'null (mapcar #'process-test-names-for-file clojure-files)))
  (format "valid: %s, pluralized: %s, prefixed: %s, missing-suffix: %s, unrecognized: %s" valid pluralized prefixed missing-suffix unrecognized))

;; (fixup-tests-for '("/home/dan/projects/clojure/cider-nrepl/test/clj/cider/nrepl/middleware/util/instrument_test.clj"))
;; (remove-if #'null (mapcar #'process-test-names-for-file clojure-files))
;;(fixup-tests-for clojure-files)
;; (fixup-tests-for '("/home/dan/projects/clojure/cider-nrepl/test/clj/cider/nrepl/middleware/util/instrument_test.clj"))

(provide 'rename_tests)

;;; rename_tests.el ends here
