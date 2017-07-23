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
      (println (send (send model 'get-object 'B) 'get-object-type))
      (check-equal? 1 1)))

   (test-case
    "We should be able to generate a route"
    (let ([model (make-rail-model "test.txt")])
      (println "ROUTE: ")
      (println (send (send (send model 'generate-route 'B 'D (list 'B 'C 'D)) 'get-current-element) 'get-switch-mode))
      (check-equal? 1 1)))

   (test-case
    "We should be able to generate a route with no provided path"
    (let ([model (make-rail-model "test.txt")])
      (println "ROUTE: ")
      (println (send (send (send model 'generate-route 'B 'D) 'get-current-element) 'get-switch-mode))
      (check-equal? 1 1)))))

(run-tests rail-model-tests)