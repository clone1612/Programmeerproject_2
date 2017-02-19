#lang racket

(require "wagon.rkt")
(require "utilities.rkt")
(require rackunit)
(require rackunit/text-ui)

(define wagon-tests
  (test-suite
   "Tests for wagon.rkt"

   (test-case
    "Wagon has wagon1 as ID - test getID"
    (let ((wagon (make-wagon "wagon1" "passengers")))
      (check-equal? (send wagon 'getID) "wagon1")))

   (test-case
    "Wagon has passengers as Type - test getType"
    (let ((wagon (make-wagon "wagon1" "passengers")))
      (check-equal? (send wagon 'getType) "passengers")))

   (test-case
    "setType! should change the wagon type as expected"
    (let ((wagon (make-wagon "wagon1" "passengers")))
      (send wagon 'setType! "coal")
      (check-equal? (send wagon 'getType) "coal")))

   (test-case
    "Wagon as 0 as initial load - test getLoad"
    (let ((wagon (make-wagon "wagon1" "passengers")))
      (check-equal? (send wagon 'getLoad) 0)))

   (test-case
    "load! should increase the load as expected"
    (let ((wagon (make-wagon "wagon1" "passengers")))
      (send wagon 'load! 150)
      (check-equal? (send wagon 'getLoad) 150)))

   (test-case
    "unload! with an acceptable amount should decrease the load as expected"
    (let ((wagon (make-wagon "wagon1" "passengers")))
      (send wagon 'load! 150)
      (send wagon 'unload! 100)
      (check-equal? (send wagon 'getLoad) 50)))

   (test-case
    "unload! with an unacceptable amount should not work"
    (let ((wagon (make-wagon "wagon1" "passengers")))
      (send wagon 'load! 150)
      (send wagon 'unload! 200)
      (check-equal? (send wagon 'getLoad) 150)))))

(run-tests wagon-tests)