(defpackage mandelbrot/tests/main
  (:use :cl
        :mandelbrot
        :rove))
(in-package :mandelbrot/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :mandelbrot)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
