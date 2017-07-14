#lang racket

(require "../utilities/utilities.rkt")

(define (nmbs)
  (let ((current-thread FALSE)
        (running? FALSE)
        (loop-wait 1.0)
        (loop-virtual-time 1.0))

    ; Start the nmbs service
    (define (start)
      (when running?
        (error "NMBS is already running."))
      (set! running? #t)
      ; TODO -> Load model)
      (set! current-thread
            (thread nmbs-loop)))

    (define (stop)
      (unless (and running? current-thread)
        (error "NMBS is not running."))
      (set! running? #f)
      (let while ()
        (unless (thread-dead? current-thread)
          (sleep 0.1)
          (while))))

    (define (nmbs-loop)
      (when running?
        ; Send needed requests
        (define-values (in out) (tcp-connect "localhost" 9883))
        (write "Ping" out)
        (flush-output out)
        (displayln (read in))
        (close-input-port in)
        (close-output-port out)
        (sleep loop-wait)
        (nmbs-loop)))

    (define (dispatch message)
      (case message
        ((start) start)
        ((stop) stop)))

    dispatch))

