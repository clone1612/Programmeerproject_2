#lang racket

(provide make-timer)
(require "utilities.rkt")
(require (prefix-in gui: racket/gui))

(define (make-timer updateFrequency object procedure)
  (let ((counter 0)
        (active FALSE)
        (timer 0)
        (updateFrequency updateFrequency)
        (object object)
        (procedure procedure))
    
    (define (start)
      (set! active TRUE)
      (set! timer (new gui:timer%
           (interval updateFrequency)
           (notify-callback
            (lambda ()
              (cond [active
                     (send object procedure)]
                    [else
                     (send timer stop)]))))))

    (define (stop)
      (set! active FALSE))

    (define (dispatch message)
      (case message
        ((start) start)
        ((stop) stop)))

    dispatch))