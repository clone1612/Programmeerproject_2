#lang racket

(require "utilities.rkt")
(require "paths.rkt")

(define (make-segment neighbours)
  (let* ((amountOfNeighbours neighbours)
         (paths (make-pathsManager (* amountOfNeighbours amountOfNeighbours))))
    
    (define (getPath d)
      (send paths 'getPath d))

    (define (addPath d p)
      (send paths 'addPath d p))

    (define (dispatch message)
      (case message
        ((getPath) getPath)
        ((addPath) addPath)))

    dispatch))

(define segment (make-segment 4))
(define path (list 1 2 3 4))
(define path2 (list 4 3 2 1))
(define destination 'A2)
(define destination2 'B2)
(send segment 'addPath destination path)
(send segment 'addPath destination2 path2)
(display (send segment 'getPath destination)) (newline)
(display (send segment 'getPath destination2)) (newline)