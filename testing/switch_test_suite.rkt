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
    "We should be able to get the path of a specific switch mode"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200))))
      (check-equal? (send switch 'get-mode-path 1) (switch-mode-info 1 "bla1" "bla2" 175))))

   (test-case
    "We should be able to get the path of a specific switch mode (given we used the optional third destination e.g. bla4)"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200) '("bla4" 225))))
      (check-equal? (send switch 'get-mode-path 3) (switch-mode-info 3 "bla1" "bla4" 225))))

   (test-case
    "We should be able to modify the current mode of a switch"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200))))
      (send switch 'set-mode! 2)
      (check-equal? (send switch 'get-mode) 2)))

   (test-case
    "We should be able to get the type of object"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200))))
      (check-equal? (send switch 'get-object-type) "switch")))

   (test-case
    "We should be able to find the required switch mode for a path to be possible"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200))))
      (check-equal? (send switch 'find-required-mode "bla1" "bla2") 1)
      (check-equal? (send switch 'find-required-mode "bla2" "bla1") 1)))

   (test-case
    "We should be able to get a writable string representing the object"
    (let ((switch (make-switch "switch-1" "bla1" '("bla2" 175) '("bla3" 200))))
      (check-equal? (send switch 'get-writable) (list "OS" "switch-1" "bla1" "bla2" "bla3"))))))

(run-tests switch-tests)