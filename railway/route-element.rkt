#lang racket

(require "../utilities/utilities.rkt")

(provide make-route-element)

(define (make-route-element type id length [mode -1])
  (let ([route-element FALSE]
        [type-index 0]
        [id-index 1]
        [length-index 2]
        [switch-index 3])

    (if (eq? mode -1)
        (set! route-element (vector type id length))
        (set! route-element (vector type id length mode)))

    ; Function that will return the type of the route element
    ; @return -> type of route element
    (define (get-type)
      (vector-ref route-element type-index))

    ; Function that will return the id of the route element
    ; @return -> id of route element
    (define (get-id)
      (vector-ref route-element id-index))

    ; Function that will return the length of the route element
    ; @return -> length of route element
    (define (get-length)
      (vector-ref route-element length-index))

    ; Function that will modify the length of the route element
    ; @param new-length -> new length of the route element
    (define (set-length! new-length)
      (vector-set! route-element length-index new-length))

    ; Function that will return the mode of the switch (optional fourth argument)
    ; @return -> switch mode
    (define (get-switch-mode)
      (if (eq? (vector-length route-element) 3)
          (error "Not a switch, cannot return switch mode")
          (vector-ref route-element switch-index)))

    (define (dispatch message)
      (case message
        ((get-type) get-type)
        ((get-id) get-id)
        ((get-length) get-length)
        ((set-length!) set-length!)
        ((get-switch-mode) get-switch-mode)))

    dispatch))

    