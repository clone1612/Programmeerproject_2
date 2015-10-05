#lang racket

(provide make-timer)
(require "utilities.rkt")
(require (prefix-in gui: racket/gui))

(define (make-timer timerLength object command)
  (let ((timerLength timerLength)
        (object object)
        (command command))

    (define timer
      (new gui:timer%
           [notify-callback ; What to do if timer expires?
            (lambda ()
              (send object command))]
           [interval FALSE])) ; FALSE to make sure timer doesn't start right away
    
    (define (start)
      (gui:send timer start timerLength))

    (define (stop)
      (gui:send timer stop))

    (define (dispatch message)
      (case message
        ((start) start)
        ((stop) stop)))

    dispatch))