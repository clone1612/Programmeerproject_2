#lang racket

(provide (all-defined-out))

(define (make-pathsManager maxAmount)
  (let ((destinations '())
        (paths (make-vector maxAmount))
        (pathCursor 0))

    ; Add a path to our pathsManager
    ; 1st Argument -> destination (to what segment does this path go?)
    ; 2nd Argument -> path (how do we reach our destination from inside this segment?)
    ; Return -> /
    (define (addPath d p)
      (set! destinations (reverse (cons d destinations)))
      (vector-set! paths pathCursor p)
      (set! pathCursor (+ pathCursor 1)))

    ; Get a path from our pathsManager
    ; 1st Argument -> destination (what segment do we want to reach from inside this segment?)
    ; Return -> list with the path to the requested destination
    (define (getPath d)
      (let loop
        ((count 0)
         (d2 destinations))
        (if (eq? (car d2) d)
            (vector-ref paths count)
            (loop (+ count 1) (cdr d2)))))

    (define (dispatch message)
      (case message
        ((getPath) getPath)
        ((addPath) addPath)))

    dispatch))