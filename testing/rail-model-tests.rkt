#lang racket

(require "../railway/rail-model.rkt")
(require "../utilities/utilities.rkt")
(require rackunit)
(require rackunit/text-ui)

(define rail-model-tests
  (test-suite
   "Tests for rail-model.rkt"

   (test-case
    "We should be able to load a file into the model"
    (let ([model (make-rail-model "test.txt")])
      (println (send (send model 'get-object 'switch1) 'get-object-type))
      (check-equal? 1 1)))))

(run-tests rail-model-tests)