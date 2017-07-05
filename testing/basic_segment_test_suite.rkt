#lang racket

(require "../railway/basic_segment.rkt")
(require "../utilities/utilities.rkt")
(require rackunit)
(require rackunit/text-ui)

(define basic-segment-tests
  (test-suite
   "Tests for basic_segment.rkt"

   (test-case
    "We should be able to get the id of a segment"
    (let ((segment (make-basic-segment 'basic-1 3500 14)))
      (check-equal? (send segment 'get-id) 'basic-1)))

   (test-case
    "We should be able to modify the id of a segment"
    (let ((segment (make-basic-segment 'basic-1 3500 14)))
      (send segment 'set-id! 'basic-2)
      (check-equal? (send segment 'get-id) 'basic-2)))

   (test-case
    "We should be able to get the length of a segment"
    (let ((segment (make-basic-segment 'basic-1 3500 14)))
      (check-equal? (send segment 'get-length) 3500)))

   (test-case
    "We should be able to modify the length of a segment"
    (let ((segment (make-basic-segment 'basic-1 3500 14)))
      (send segment 'set-length! 3750)
      (check-equal? (send segment 'get-length) 3750)))

   (test-case
    "We should be able to get the maximum speed of a segment"
    (let ((segment (make-basic-segment 'basic-1 3500 14)))
      (check-equal? (send segment 'get-max-speed) 14)))

   (test-case
    "We should be able to modify the maximum speed of a segment"
    (let ((segment (make-basic-segment 'basic-1 3500 14)))
      (send segment 'set-max-speed! 15)
      (check-equal? (send segment 'get-max-speed) 15)))))

(run-tests basic-segment-tests)