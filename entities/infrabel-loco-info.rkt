#lang racket

(require "../utilities/utilities.rkt")

(provide make-infrabel-loco-info)

(define (make-infrabel-loco-info)
  (let ([previous-location FALSE]
        [current-location FALSE]
        [red-lights FALSE])

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

    ; Function that will return the list of red lights activated by this locomotive
    ; @return -> red lights activated by this locomotive (list)
    (define (get-red-lights)
      red-lights)

    ; Function that will modify the list of red lights activated by this locomotive
    ; @param -> red lights (list)
    (define (set-red-lights! new-red-lights)
      (set! red-lights new-red-lights))

    (define (dispatch message)
      (case message
        ((get-previous-location) get-previous-location)
        ((set-previous-location!) set-previous-location!)
        ((get-current-location) get-current-location)
        ((set-current-location!) set-current-location!)
        ((get-red-lights) get-red-lights)
        ((set-red-lights!) set-red-lights!)))

    dispatch))