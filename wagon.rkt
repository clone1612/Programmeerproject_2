#lang racket

(provide make-wagon)

(define (make-wagon id type)
  (let ((wagonID id)
        (wagonType type))

    (define (getID)
      wagonID)

    (define (getType)
      wagonType)

    (define (dispatch message)
      (case message
        ((getID) getID)
        ((getType) getType)))

    dispatch))