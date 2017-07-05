#lang racket

(provide make-segmentConnection)

; ADT representing a connection between segments with a switch
; @param fromSegment: ID of the start object of a connection
; @param toSegment: ID of the end object of a connection
(define (make-segmentConnection fromSegment toSegment)
  (let ((from fromSegment)
        (to toSegment))

    ; Function that will return the start of a connection
    ; @return ID: String with the ID of the start object
    (define (getFrom)
      from)

    ; Function that will return the end of a connection
    ; @return ID: String with the ID of the end object
    (define (getTo)
      to)

    ; Function that will change the start of a connection
    ; @param newFrom: ID of the new start of a connection
    (define (setFrom! newFrom)
      (set! from newFrom))

    ; Function that will change the end of a connection
    ; @param newTo: ID of the new end of a connection
    (define (setTo! newTo)
      (set! to newTo))

    (define (dispatch message)
      (case message
        ((getFrom) getFrom)
        ((getTo) getTo)
        ((setFrom!) setFrom!)
        ((setTo!) setTo!)
        (else (displayln "segmentConnection: Unknown message..."))))

    dispatch))