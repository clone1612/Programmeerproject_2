#lang racket

(require "../utilities/utilities.rkt")

; ADT representing a route, in our case a list of the three types of railway elements

(define (make-route)
  (let ([route FALSE])

    (define (add-to-route route-part)
      (if route
          (set! route (append route (list route-part)))
          (set! route (list route-part)))
      (println route))

    ; Function that will add a basic segment to our route
    ; @param id -> id of the basic segment
    ; @param length -> length of the basic segment
    (define (add-basic-segment id length)
      (let ([route-part (box (list 'B id length))])
        (add-to-route route-part)))

    ; Function that will add a detection block to our route
    ; @param id -> id of the detection block
    ; @param length -> length of the detection block
    (define (add-detection-block id length)
      (let ([route-part (box (list 'D id length))])
        (add-to-route route-part)))

    ; Function that will add a switch to our route
    ; @param id -> id of the switch
    ; @param length -> length of the switch
    ; @param mode -> mode the switch needs to be in for our route to work
    (define (add-switch id length mode)
      (let ([route-part (box (list 'S id length mode))])
        (add-to-route route-part)))

    ; Function that will return the current part of the route as a list
    ; @return -> current route-part (list)
    (define (get-current-part)
      (unbox (car route)))

    (define (dispatch message)
      (case message
        ((add-basic-segment) add-basic-segment)
        ((add-detection-block) add-detection-block)
        ((add-switch) add-switch)
        ((get-current-part) get-current-part)))

    dispatch))

(define test (make-route))
(send test 'add-basic-segment "a" 2500)
(send test 'add-basic-segment "b" 3500)
(send test 'add-detection-block "ddd" 250)
(send test 'add-switch "sss" 750 2)
(send test 'get-current-part)