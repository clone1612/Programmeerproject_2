#lang racket

(require "../railway/route-element.rkt")
(require "../utilities/utilities.rkt")
(require rackunit)
(require rackunit/text-ui)

(define route-element-tests
  (test-suite
   "Tests for route-element.rkt"

   (test-case
    "We should be able to get the type of a route element"
    (let ([route-element (make-route-element 'detection 'det1 275)])
      (check-equal? (send route-element 'get-type) 'detection)))

   (test-case
    "We should be able to get the id of a route element"
    (let ([route-element (make-route-element 'detection 'det1 275)])
      (check-equal? (send route-element 'get-id) 'det1)))

   (test-case
    "We should be able to get the length of a route element"
    (let ([route-element (make-route-element 'detection 'det1 275)])
      (check-equal? (send route-element 'get-length) 275)))

   (test-case
    "We should be able to modify the length of a route element"
    (let ([route-element (make-route-element 'detection 'det1 275)])
      (send route-element 'set-length! 250)
      (check-equal? (send route-element 'get-length) 250)))

   (test-case
    "We should be able to get the switch mode if the route element represents a switch"
    (let ([route-element (make-route-element 'switch 'swi1 350 2)])
      (check-equal? (send route-element 'get-switch-mode) 2)))))

(run-tests route-element-tests)