#lang racket

(require "segmentConnection.rkt")
(require "utilities.rkt")
(require rackunit)
(require rackunit/text-ui)

(define segment-connection-tests
  (test-suite
   "Tests for segmentConnection.rkt"

   (test-case
    "From should equal to first argument after creation"
    (let ((connection (make-segmentConnection "seg_1" "seg_2")))
      (check-equal? (send connection 'getFrom) "seg_1")))

   (test-case
    "To should equal to second argument after creation"
    (let ((connection (make-segmentConnection "seg_1" "seg_2")))
      (check-equal? (send connection 'getTo) "seg_2")))

   (test-case
    "Changing _From_ should work as expected"
    (let ((connection (make-segmentConnection "seg_1" "seg_2")))
      (send connection 'setFrom! "seg_3")
      (check-equal? (send connection 'getFrom) "seg_3")))

   (test-case
    "Changing _To_ should work as expected"
    (let ((connection (make-segmentConnection "seg_1" "seg_2")))
      (send connection 'setTo! "seg_4")
      (check-equal? (send connection 'getTo) "seg_4")))))

(run-tests segment-connection-tests)
