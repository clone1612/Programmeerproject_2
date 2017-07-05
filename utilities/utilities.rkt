#lang racket

(provide (all-defined-out))

(define TRUE #t)
(define FALSE #f)

(define (send object message . parameters)
  (let ((procedure (object message))) 
    (if (not (void? procedure))
        (apply procedure parameters)
        (display (string-append "ERROR - Object does not recognize given message -> " (symbol->string message))))))

(define (print-test testSuite testName testResult)
  (display (string-append testName " --> " testResult " (" testSuite ")")))