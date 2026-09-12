(defpackage portfolio-cl/tests/main
  (:use :cl
        :portfolio-cl
        :rove))
(in-package :portfolio-cl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :portfolio-cl)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
