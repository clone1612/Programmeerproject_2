#lang racket

(provide make-signal)

; ADT representing a signal that can be present on a railroad
; Input: id (unique identifier for the signal)

(define (make-signal id)
  (let* ((signalID id)
         (statusVector (vector "green" "orange" "red"))
         (signalStatus (vector-ref statusVector 0)))

    ; Function that will return the ID of the signal
    ; Input: /
    ; Output: signalID (ID of the signal)
    (define (getID)
      signalID)

    ; Function will return the current status of the signal
    ; Input: /
    ; Output: signalStatus (status of the signal - string)
    (define (getStatus)
      signalStatus)

    ; Function that will change the status of the signal to a valid new one
    ; Input: newStatus (a valid new status for the signal - string)
    ; Output: /
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