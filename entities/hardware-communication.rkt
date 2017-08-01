#lang racket

(require "../utilities/utilities.rkt")
(require "../simulator/interface.rkt") ; Simulator

(provide make-hardware-communication)

(define (make-hardware-communication hardware-active)
  (let ([simulator (not hardware-active)])

    (when simulator
      (start-simulator))

    ; Function that will ask the hardware to return the speed of a locomotive
    ; @param id -> id of the locomotive (symbol)
    ; @return -> speed of the locomotive (number)
    (define (get-speed id)
      (if simulator
          (get-loco-speed id)
          "todo"))

    ; Function that will ask the hardware to update the speed of a locomotive
    ; @param id -> id of the locomotive
    ; @param new-speed -> new-speed
    (define (set-speed! id new-speed)
      (if simulator
          (set-loco-speed! id new-speed)
          "todo"))

    ; Function that will return the current detection block of a locomotive
    ; @param id -> id of the locomotive (symbol)
    ; @return -> detection block of the locomotive (symbol)
    (define (get-detection-block id)
      (if simulator
          (get-loco-detection-block id)
          "todo"))

    ; Function that will return the current state of a switch
    ; @param id -> id of the switch (symbol)
    ; @return -> state of the switch (number)
    (define (get-switch-state id)
      (if simulator
          (get-switch-position id)
          "todo"))

    ; Function that will modify the state of a switch
    ; @param id -> id of the switch (symbol)
    ; @param new-state -> new state of the switch (number)
    (define (set-switch-state! id new-state)
      (if simulator
          (set-switch-position! id new-state)
          "todo"))

    (define (dispatch message)
      (case message
        ((set-speed!) set-speed!)
        ((get-speed) get-speed)
        ((get-detection-block) get-detection-block)
        ((get-switch-state) get-switch-state)
        ((set-switch-state!) set-switch-state!)))

    dispatch))