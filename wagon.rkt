#lang racket

(provide make-wagon)

; ADT representing a wagon that can be attached to a locomotive
; Input 1: id (unique identifier for the wagon)
; Input 2: type (the type of load that can be loaded into this wagon

(define (make-wagon id type)
  (let ((wagonID id)
        (wagonType type)
        (wagonLoad 0))

    ; Function that will return the ID of the wagon
    ; Input: /
    ; Output wagonID (ID of the wagon)
    (define (getID)
      wagonID)

    ; Function that will return the type of load the wagon can transport
    ; Input: /
    ; Output: wagonType (type of the wagon)
    (define (getType)
      wagonType)

    ; Function that will change the type of the wagon to a new type
    ; Input: newType (new type of load the wagon can transport)
    ; Output: /
    (define (setType! newType)
      (set! wagonType newType))

    ; Function that will return the current load of the wagon
    ; Input: /
    ; Output: wagonLoad (current load of the wagon - number)
    (define (getLoad)
      wagonLoad)

    ; Function that will load a certain amount into the wagon
    ; Input: amount (how much will be loaded into the wagon - number)
    ; Output: /
    (define (load! amount)
      (set! wagonLoad (+ wagonLoad amount)))

    ; Function that will unload a certain amount from the wagon
    ; Input: amount (how much will be unloaded out of the wagon - number)
    ; Output: /
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