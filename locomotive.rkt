#lang racket

(require "utilities.rkt")
(require "timer.rkt")

(define (make-locomotive position wagons)
  (let ((locoPosition position)
        (locoSpeed 0)
        (locoWagons wagons))

    (define (getPosition)
      locoPosition)

    (define (updatePosition)
      (let ((x (car locoPosition))
            (y (cdr locoPosition)))
        (set! locoPosition (cons (+ x 1) (+ y 1)))
        (display locoPosition)))

    (define (getSpeed)
      locoSpeed)

    (define (setSpeed! newSpeed)
      (set! locoSpeed newSpeed))

    (define (getWagons)
      locoWagons)

    (define (dispatch message)
      (case message
        ((getPosition) getPosition)
        ((updatePosition) updatePosition)
        ((getSpeed) getSpeed)
        ((setSpeed!) setSpeed!)
        ((getWagons) getWagons)))

    dispatch))

(define position (cons 0 0))
(define wagons (list 1 2 3 4 5))
(define train (make-locomotive position wagons))
(define timer (make-timer 100 train 'updatePosition))
(send timer 'start)
(display (send train 'getPosition)) (newline)
(display (send train 'getSpeed)) (newline)
(send train 'setSpeed! 50)
(display (send train 'getSpeed))