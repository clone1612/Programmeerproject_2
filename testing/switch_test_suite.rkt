#lang racket

(require "../railway/switch.rkt")
(require "../utilities/utilities.rkt")
(require rackunit)
(require rackunit/text-ui)

(define switch-tests
  (test-suite
   "Tests for switch.rkt"

   (test-case
    "We should be able to get the id of a switch"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200))))
      (check-equal? (send switch 'get-id) "switch-1")))

   (test-case
    "We should be able to modify the id of a switch"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200))))
      (send switch 'set-id! "switch-2")
      (check-equal? (send switch 'get-id) "switch-2")))

   (test-case
    "We should be able to get the start of a switch"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200))))
      (check-equal? (send switch 'get-start) "bla1")))

   (test-case
    "We should be able to get the current mode of a switch"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200))))
      (check-equal? (send switch 'get-mode) 1)))

   (test-case
    "We should be able to get information about a specific switch mode"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200))))
      (check-equal? (send switch 'get-mode-info 1) (switch-mode-info 1 "bla1" "bla2" 175))))

   (test-case
    "We should be able to get information about a specific switch mode (given we used the optional third destination e.g. bla4)"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200) '("bla4" 225))))
      (check-equal? (send switch 'get-mode-info 3) (switch-mode-info 3 "bla1" "bla4" 225))))

   (test-case
    "We should be able to modify the current mode of a switch"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200))))
      (send switch 'set-mode! 2)
      (check-equal? (send switch 'get-mode) 2)))))

(run-tests switch-tests)