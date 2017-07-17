#lang racket

(provide make-locomotive)

; ADT representing a locomotive
; @param front-node -> ID of the node the locomotive is facing
; @param back-node -> ID of the node to the back of the locomotive
(define (make-locomotive front-node back-node)
  (let ((front front-node)
        (back back-node)
        (position front-node)
        (speed 0))

    (define (get-position)
      position)

    (define (set-position! new-position)
      (set! position new-position))

    (define (get-speed)
      speed)

    (define (set-speed! new-speed)
      (set! speed new-speed))

    (define (dispatch message)
      (case message
        ((get-position) get-position)
        ((set-position!) set-position!)
        ((get-speed) get-speed)
        ((set-speed!) set-speed!)))

    dispatch))