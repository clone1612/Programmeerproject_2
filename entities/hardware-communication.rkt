#lang racket

(require "../utilities/utilities.rkt")
(require "../simulator/interface.rkt") ; Simulator

(provide make-hardware-communication)

(define (make-hardware-communication hardware-active)
  (let ([simulator (not hardware-active)])

    (when simulator
      (start-simulator))

    ; Function that will ask the hardware to update the speed of a locomotive
    ; @param id -> id of the locomotive
    ; @param new-speed -> new-speed
    (define (set-speed! id new-speed)
      (if simulator
          (set-loco-speed! id new-speed)
          "todo"))

    (define (dispatch message)
      (case message
        ((set-speed!) set-speed!)))

    dispatch))