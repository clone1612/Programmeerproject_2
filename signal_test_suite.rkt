#lang racket

(require "signal.rkt")
(require "utilities.rkt")

(define (signal-test-suite)

  (define (run-test-suite)
    (let ((signal (make-signal "signal1"))
          (success TRUE)
          (failedString "Test failed: "))

      ; Tests the "getID" command
      (unless (equal? (send signal 'getID) "signal1")
        (set! success FALSE)
        (displayln (string-append failedString "getID")))

      ; Tests the "getStatus" command
      (unless (equal? (send signal 'getStatus) "green")
        (set! success FALSE)
        (displayln (string-append failedString "getStatus")))

      ; Tests the "setStatus!" command - Acceptable newStatus should work
      (send signal 'setStatus! "orange")

      (unless (equal? (send signal 'getStatus) "orange")
        (set! success FALSE)
        (displayln (string-append failedString "setStatus!")))

      ; Tests the "setStatus!" command - Unacceptable newStatus should not work
      (send signal 'setStatus! "purple")

      (unless (equal? (send signal 'getStatus) "orange")
        (set! success FALSE)
        (displayln (string-append failedString "setStatus!")))

      (if success
          (displayln "All tests run with success!")
          (displayln "Some test(s) failed"))))

  (define (dispatch message)
      (case message
        ((run-test-suite) run-test-suite)
        (else (displayln "SIGNAL TEST SUITE: Unknown message..."))))

  dispatch)

(define test (signal-test-suite))
(send test 'run-test-suite)