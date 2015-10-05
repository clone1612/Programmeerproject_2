#lang racket

(require "utilities.rkt")
(require "timer.rkt")

(define (make-locomotive position wagons)
  (let ((trainPosition position)
        (trainSpeed 0)
        (trainWagons wagons))

    (define (getPosition)
      trainPosition)

    (define (updatePosition)
      (let ((x (car trainPosition))
            (y (cdr trainPosition)))
        (set! trainPosition (cons (+ x 1) (+ y 1)))
        (display trainPosition)))

    (define (getSpeed)
      trainSpeed)

    (define (setSpeed! newSpeed)
      (set! trainSpeed newSpeed))

    (define (getWagons)
      wagons)

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