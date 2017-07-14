#lang racket

(require "../utilities/utilities.rkt")

(define (infrabel)
  (let ((current-thread FALSE)
        (running? FALSE)
        (loop-wait 1.0)
        (loop-virtual-time 1.0)
        (tcp-listener FALSE))

    ; Start the infrabel service
    (define (start)
      (when running?
        (error "Infrabel is already running."))
      (set! running? #t)
      ; TODO -> Load model
      (set! tcp-listener
            (tcp-listen 9883 4 TRUE))
      (set! current-thread
            (thread infrabel-loop)))

    (define (stop)
      (unless (and running? current-thread)
        (error "Infrabel is not running."))
      (set! running? #f)
      (tcp-close tcp-listener)
      (let while ()
        (unless (thread-dead? current-thread)
          (sleep 0.1)
          (while))))

    (define (test a b)
      (displayln a)
      (displayln b))

    (define (infrabel-loop)
      (when running?
        ; Listen to incoming requests
        (when (tcp-accept-ready? tcp-listener)
          (define-values (in out) (tcp-accept tcp-listener))
          (let ((received-command (read in)))
            (displayln received-command)
            (write "Pong" out)
            (flush-output out)
            (close-input-port in)
            (close-output-port out)
            (sleep loop-wait)
            (infrabel-loop)))
        (sleep loop-wait)
        (infrabel-loop)))

    (define (dispatch message)
      (case message
        ((start) start)
        ((stop) stop)))

    dispatch))