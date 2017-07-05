#lang racket

(require "../railway/detection_block.rkt")
(require "../utilities/utilities.rkt")
(require rackunit)
(require rackunit/text-ui)

(define detection-block-tests
  (test-suite
   "Tests for detection_block.rkt"

   (test-case
    "We should be able to get the id of a detection block"
    (let ((block (make-detection-block 'detection-1 3500 14)))
      (check-equal? (send block 'get-id) 'detection-1)))

   (test-case
    "We should be able to modify the id of a detection block"
    (let ((block (make-detection-block 'detection-1 3500 14)))
      (send block 'set-id! 'detection-2)
      (check-equal? (send block 'get-id) 'detection-2)))

   (test-case
    "We should be able to get the length of a detection block"
    (let ((block (make-detection-block 'detection-1 3500 14)))
      (check-equal? (send block 'get-length) 3500)))

   (test-case
    "We should be able to modify the length of a detection block"
    (let ((block (make-detection-block 'detection-1 3500 14)))
      (send block 'set-length! 3750)
      (check-equal? (send block 'get-length) 3750)))

   (test-case
    "We should be able to get the maximum speed of a detection block"
    (let ((block (make-detection-block 'detection-1 3500 14)))
      (check-equal? (send block 'get-max-speed) 14)))

   (test-case
    "We should be able to modify the maximum speed of a detection block"
    (let ((block (make-detection-block 'detection-1 3500 14)))
      (send block 'set-max-speed! 15)
      (check-equal? (send block 'get-max-speed) 15)))))

(run-tests detection-block-tests)