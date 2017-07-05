#lang racket

(provide make-basic-segment)

(define (make-basic-segment _id _length _max-speed)
  (let ((id _id)
        (length _length)
        (max-speed _max-speed))

    ; Function that will return the id of the segment
    ; @return -> id
    (define (get-id)
      id)

    ; Function that will set the id to a new value
    ; @param new-id -> New id of the segment
    (define (set-id! new-id)
      (set! id new-id))

    ; Function that will return the length (in m) of the segment
    ; @return -> length (in m)
    (define (get-length)
      length)

    ; Function that will set the length to a new value
    ; @param new-length -> New length of the segment (in m)
    (define (set-length! new-length)
      (set! length new-length))

    ; Function that will return the max speed (in m/s) of the segment
    ; @return -> max-speed (in m/s)
    (define (get-max-speed)
      max-speed)

    ; Function that will set the max speed to a new value
    ; @param new-max-speed -> New max speed of the segment (in m/s)
    (define (set-max-speed! new-max-speed)
      (set! max-speed new-max-speed))

    (define (dispatch message)
      (case message
        ((get-id) get-id)
        ((set-id!) set-id!)
        ((get-length) get-length)
        ((set-length!) set-length!)
        ((get-max-speed) get-max-speed)
        ((set-max-speed!) set-max-speed!)
        (else (displayln "BASIC-SEGMENT: Unknown message..."))))

    dispatch))