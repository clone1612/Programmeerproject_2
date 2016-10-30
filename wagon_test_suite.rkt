#lang racket

(require "wagon.rkt")
(require "utilities.rkt")

(define (wagon-test-suite)

  (define (run-test-suite)
    (let ((wagon (make-wagon "wagon1" "passengers"))
          (success TRUE)
          (failureList '())
          (failedString "Test failed: "))

      ; Tests the "getID" command
      (unless (equal? (send wagon 'getID) "wagon1")
        (set! success FALSE)
        (displayln (string-append failedString "getID")))

      ; Tests the "getType" command
      (unless (equal? (send wagon 'getType) "passengers")
        (set! success FALSE)
        (displayln (string-append failedString "getType")))

      ; Tests the "setType!" command
      (send wagon 'setType! "coal")

      (unless (equal? (send wagon 'getType) "coal")
        (set! success FALSE)
        (displayln (string-append failedString "setType!")))

      ; Tests the "getLoad" command
      (unless (equal? (send wagon 'getLoad) 0)
        (set! success FALSE)
        (displayln (string-append failedString "getLoad")))

      ; Tests the "load!" command
      (send wagon 'load! 150)

      (unless (equal? (send wagon 'getLoad) 150)
        (set! success FALSE)
        (displayln (string-append failedString "load!")))

      ; Tests the "unload!" command - Acceptable unload amount should work
      (send wagon 'unload! 50)

      (unless (equal? (send wagon 'getLoad) 100)
        (set! success FALSE)
        (displayln (string-append failedString "unload!")))

      ; Tests the "unload!" command - Unacceptable unload amount should not work
      (send wagon 'unload! 150)

      (unless (equal? (send wagon 'getLoad) 100)
        (set! success FALSE)
        (displayln (string-append failedString "unload!")))

      (if success
          (displayln "All tests run with success!")
          (displayln "Some test(s) failed"))))

  (define (dispatch message)
      (case message
        ((run-test-suite) run-test-suite)
        (else (displayln "WAGON TEST SUITE: Unknown message..."))))

  dispatch)

(define test (wagon-test-suite))
(send test 'run-test-suite)