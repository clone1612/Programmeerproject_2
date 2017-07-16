#lang racket

(provide make-switch
         (struct-out switch-mode-info))

(struct switch-mode-info (mode from to distance) #:transparent)

(define (make-switch _id _start _to-1 _to-2 [_to-3 '()])
  (let ((id _id)
        (start _start)
        (to '())
        (mode 1)
        (object-type "switch"))

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

    ; Function that will return the path of a given mode
    ; @param mode -> mode we want to know the path of
    ; @return -> path (list)
    (define (get-mode-path mode)
      (switch-mode-info mode start (car (vector-ref to (- mode 1))) (cadr (vector-ref to (- mode 1)))))

    ; Function that will return the mode required to go from 'from' to 'to'
    ; @param _from -> starting point of the route we want to take
    ; @param _to -> end point of the route we want to take
    ; @return -> mode this switch needs to be in for that route to be possible
    (define (find-required-mode _from _to)
      (define result -1)
      (for ([i (vector-length to)])
        (let ([possible (car (vector-ref to i))])
          (when (or (equal? possible _to) (equal? possible _from))
              (set! result (+ i 1)))))
      result)

    ; Function that will modify the mode of the switch
    ; @param new-mode -> new mode of the switch
    (define (set-mode! new-mode)
      (set! mode new-mode))

    ; Function that will return the type of object this represents
    ; @return -> object type (string)
    (define (get-object-type)
      object-type)

    (define (dispatch message)
      (case message
        ((get-id) get-id)
        ((set-id!) set-id!)
        ((get-start) get-start)
        ((get-mode) get-mode)
        ((get-mode-path) get-mode-path)
        ((set-mode!) set-mode!)
        ((get-object-type) get-object-type)
        ((find-required-mode) find-required-mode)
        (else (displayln "SWITCH: Unknown message..."))))

    dispatch))
  