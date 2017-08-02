#lang racket

(provide make-locomotive)

; ADT representing a locomotive
; @param current-block -> ID of the detection block the locomotive is currently on
; @param previous-block -> ID of the detection block to the back of the locomotive faces
(define (make-locomotive current-block previous-block)
  (let ([current-location current-block]
        [previous-location previous-block]
        [speed 0])

    ; Function that will return the previous location of a locomotive
    ; @return -> id of previous location (symbol)
    (define (get-previous-location)
      previous-location)

    ; Function that will modify the previous location of a locomotive
    ; @param new-previous-location -> id of the new previous location (symbol)
    (define (set-previous-location! new-previous-location)
      (set! previous-location new-previous-location))

    ; Function that will return the current location of a locomotive
    ; @return -> id of the current location (symbol)
    (define (get-current-location)
      current-location)

    ; Function that will modify the current location of a locomotive
    ; @param new-current-location -> id of the new current location (symbol)
    (define (set-current-location! new-current-location)
      (set! current-location new-current-location))

    ; Function that will return the speed of the locomotive
    ; @return -> speed of the locomotive (number)
    (define (get-speed)
      speed)

    ; Function that will modify the speed of the locomotive
    ; @param new-speed -> new speed of the locomotive
    (define (set-speed! new-speed)
      (set! speed new-speed))

    (define (dispatch message)
      (case message
        ((get-previous-location) get-previous-location)
        ((set-previous-location!) set-previous-location!)
        ((get-current-location) get-current-location)
        ((set-current-location!) set-current-location!)
        ((get-speed) get-speed)
        ((set-speed!) set-speed!)))

    dispatch))