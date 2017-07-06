#lang racket

(provide make-switch
         (struct-out switch-mode-info))

(struct switch-mode-info (mode from to distance) #:transparent)

(define (make-switch _id _start _to-1 _to-2 [_to-3 '()])
  (let ((id _id)
        (start _start)
        (to '())
        (mode 1))

    (if (eq? _to-3 '())
        (set! to (vector _to-1 _to-2))
        (set! to (vector _to-1 _to-2 _to-3)))

    ; Function that will return the id of a switch
    ; @return -> id
    (define (get-id)
      id)

    ; Function that will modify the id of a switch
    ; @param new-id -> new id of the switch
    (define (set-id! new-id)
      (set! id new-id))

    ; Function that will return the start of a switch
    ; @return -> start
    (define (get-start)
      start)

    ; Function that will return the current mode of the switch
    ; @return -> mode (int)
    (define (get-mode)
      mode)

    ; Function that will return information about a given mode
    ; @return -> mode information (mode start to length)
    (define (get-mode-info mode)
      (let ((to-info (vector-ref to (- mode 1))))
        (switch-mode-info mode start (car to-info) (cadr to-info))))

    ; Function that will modify the mode of the switch
    ; @param new-mode -> new mode of the switch
    (define (set-mode! new-mode)
      (set! mode new-mode))

    (define (dispatch message)
      (case message
        ((get-id) get-id)
        ((set-id!) set-id!)
        ((get-start) get-start)
        ((get-mode) get-mode)
        ((get-mode-info) get-mode-info)
        ((set-mode!) set-mode!)
        (else (displayln "SWITCH: Unknown message..."))))

    dispatch))
  