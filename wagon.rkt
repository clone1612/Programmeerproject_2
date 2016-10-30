#lang racket

(provide make-wagon)

(define (make-wagon id type)
  (let ((wagonID id)
        (wagonType type)
        (wagonLoad 0))

    (define (getID)
      wagonID)

    (define (getType)
      wagonType)

    (define (setType! newType)
      (set! wagonType newType))

    (define (getLoad)
      wagonLoad)

    (define (load! amount)
      (set! wagonLoad (+ wagonLoad amount)))

    (define (unload! amount)
      (let ((newLoad (- wagonLoad amount)))
        (unless (> 0 newLoad)
          (set! wagonLoad newLoad))))

    (define (dispatch message)
      (case message
        ((getID) getID)
        ((getType) getType)
        ((setType!) setType!)
        ((getLoad) getLoad)
        ((load!) load!)
        ((unload!) unload!)
        (else (displayln "WAGON: Unknown message..."))))

    dispatch))