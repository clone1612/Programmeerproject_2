#lang racket

(require "utilities.rkt")
(require "timer.rkt")
(require "wagon.rkt")

(define (make-locomotive position wagons)
  (let* ((locoPosition position)
         (locoSpeed 0)
         (locoWagons wagons)
         (locoLength (+ 1 (length locoWagons))))

    (define (getXPosition)
      (car locoPosition))

    (define (getYPosition)
      (cdr locoPosition))

    (define (updatePosition)
      (let ((x (car locoPosition))
            (y (cdr locoPosition)))
        (set! locoPosition (cons (+ x 1) (+ y 1)))
        (display locoPosition)))

    (define (getSpeed)
      locoSpeed)

    (define (setSpeed! newSpeed)
      (set! locoSpeed newSpeed))

    (define (getLength)
      locoLength)

    (define (getWagons)
      locoWagons)

    (define (dispatch message)
      (case message
        ((getXPosition) getXPosition)
        ((getYPosition) getYPosition)
        ((updatePosition) updatePosition)
        ((getSpeed) getSpeed)
        ((setSpeed!) setSpeed!)
        ((getLength) getLength)
        ((getWagons) getWagons)))

    dispatch))

(define position (cons 0 0))
(define wagon1 (make-wagon 1 'kool))
(define wagon2 (make-wagon 2 'kool))
(define wagons (list wagon1 wagon2))
(define train (make-locomotive position wagons))
;(define timer (make-timer 100 train 'updatePosition))
;(send timer 'start)
(display (send train 'getPosition)) (newline)
(display (send train 'getSpeed)) (newline)
(send train 'setSpeed! 50)
(display (send train 'getSpeed)) (newline)
(display "Lengte v/d trein = ") (display (send train 'getLength))