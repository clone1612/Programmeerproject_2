#lang racket

(require "../utilities/utilities.rkt")
(require "../railway/route-element.rkt")

(provide make-route)

; ADT representing a route, in our case a list of the three types of railway elements

(define (make-route)
  (let ([route FALSE])

    (define (add-to-route route-element)
      (if route
          (set! route (append route (list route-element)))
          (set! route (list route-element)))
      (println route))

    ; Function that will add a basic segment to our route
    ; @param id -> id of the basic segment
    ; @param length -> length of the basic segment
    (define (add-basic-segment id length)
      (let ([route-element (make-route-element 'basic id length)])
        (add-to-route route-element)))

    ; Function that will add a detection block to our route
    ; @param id -> id of the detection block
    ; @param length -> length of the detection block
    (define (add-detection-block id length)
      (let ([route-element (make-route-element 'detection id length)])
        (add-to-route route-element)))

    ; Function that will add a switch to our route
    ; @param id -> id of the switch
    ; @param length -> length of the switch
    ; @param mode -> mode the switch needs to be in for our route to work
    (define (add-switch id length mode)
      (let ([route-element (make-route-element 'switch id length mode)])
        (add-to-route route-element)))

    ; Function that will return the current part of the route as a list
    ; @return -> current route-element (route-element)
    (define (get-current-element)
      (car route))

    ; Function that will return the next part of the route as a list
    ; @return -> next route-element (route-element)
    (define (get-next-element)
      (cadr route))

    (define (dispatch message)
      (case message
        ((add-basic-segment) add-basic-segment)
        ((add-detection-block) add-detection-block)
        ((add-switch) add-switch)
        ((get-current-element) get-current-element)
        ((get-next-element) get-next-element)))

    dispatch))

(define test (make-route))
(send test 'add-basic-segment "a" 2500)
(send test 'add-basic-segment "b" 3500)
(send test 'add-detection-block "ddd" 250)
(send test 'add-switch "sss" 750 2)
(send test 'get-current-element)
(send test 'get-next-element)