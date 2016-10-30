#lang racket

(provide make-signal)

(define (make-signal id)
  (let* ((signalID id)
         (statusVector (vector "green" "orange" "red"))
         (signalStatus (vector-ref statusVector 0)))

    (define (getID)
      signalID)

    (define (getStatus)
      signalStatus)

    (define (setStatus! newStatus)
      (unless (not (vector-member newStatus statusVector))
        (set! signalStatus newStatus)))

    (define (dispatch message)
      (case message
        ((getID) getID)
        ((getStatus) getStatus)
        ((setStatus!) setStatus!)
        (else (displayln "SIGNAL: Unknown message..."))))

    dispatch))