#lang racket

(require "../utilities/utilities.rkt")
(require "../railway/rail-model.rkt")

(define (infrabel)
  (let ((current-thread FALSE)
        (running? FALSE)
        (loop-wait 1.0)
        (loop-virtual-time 1.0)
        (tcp-listener FALSE)
        (model-filename "../testing/test.txt")
        (model FALSE)
        (success-string "DONE"))

    ; Start the infrabel service
    (define (start)
      (when running?
        (error "Infrabel is already running."))
      (set! running? #t)
      ; Load model
      (set! model (make-rail-model model-filename))
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

    (define (get-loco-speed id)
      (let ([loco (send model 'get-object id)])
        (send loco 'get-speed)))

    (define (set-loco-speed! id new-speed)
      (let ([loco (send model 'get-object id)])
        ; TODO - Send to hardware/simulator
        ;
        ; Update the model
        (send loco 'set-speed! new-speed)
        (send model 'set-object! id loco)
        success-string))

    ; Helper function that will process received commands
    ; @param command -> command we want to process
    ; @return -> result of the processing
    (define (process-command command)
      ; Parse the command
      (case (string->symbol (car command))
        [(set-speed!) (let ([id (string->symbol (list-ref command 1))]
                            [new-speed (string->number (list-ref command 2))])
                        (set-loco-speed! id new-speed))]
        [(get-speed) (let ([id (string->symbol (list-ref command 1))])
                       (get-loco-speed id))]))

    (define (infrabel-loop)
      (when running?
        ; Listen to incoming requests
        (when (tcp-accept-ready? tcp-listener)
          (define-values (in out) (tcp-accept tcp-listener))
          (let* ((received-command (read in))
                 (processed-command (process-command (string-split received-command))))
            (displayln received-command)
            (write processed-command out)
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