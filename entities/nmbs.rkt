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

    ; Helper function that will send a command to infrabel
    ; @param command -> command we want to send to infrabel
    (define (send-command command)
      (define-values (in out) (tcp-connect "localhost" 9883))
      (write command out)
      (flush-output out)
      (displayln (read in))
      (close-input-port in)
      (close-output-port out))

    (define (nmbs-loop)
      (when running?
        ; TODO - For all trains with a route do some stuff
        ;
        (sleep loop-wait)
        (nmbs-loop)))

    (define (dispatch message)
      (case message
        ((send-command) send-command)
        ((start) start)
        ((stop) stop)))

    dispatch))

