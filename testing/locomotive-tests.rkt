#lang racket

(require "../utilities/utilities.rkt")
(require "../railway/locomotive.rkt")
(require rackunit)
(require rackunit/text-ui)

(define locomotive-tests
  (test-suite
   "Tests for locomotive.rkt"

   (test-case
    "We should be able to get the position of a locomotive"
    (let ([loco (make-locomotive '1 '2)])
      (check-equal? (send loco 'get-position) '1)))

   (test-case
    "We should be able to modify the position of a locomotive"
    (let ([loco (make-locomotive '1 '2)])
      (send loco 'set-position! '3)
      (check-equal? (send loco 'get-position) '3)))

   (test-case
    "We should be able to get the speed of a locomotive"
    (let ([loco (make-locomotive '1 '2)])
      (check-equal? (send loco 'get-speed) 0)))

   (test-case
    "We should be able to modify the speed of a locomotive"
    (let ([loco (make-locomotive '1 '2)])
      (send loco 'set-speed! 5)
      (check-equal? (send loco 'get-speed) 5)))))

(run-tests locomotive-tests)