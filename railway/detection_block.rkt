#lang racket

(provide make-detection-block)

(define (make-detection-block _id _length _max-speed)
  (let ((id _id)
        (length _length)
        (max-speed _max-speed)
        (object-type "detection-block"))

    ; Function that will return the id of the detection block
    ; @return -> id
    (define (get-id)
      id)

    ; Function that will set the id to a new value
    ; @param new-id -> New id of the detection block
    (define (set-id! new-id)
      (set! id new-id))

    ; Function that will return the length (in m) of the detection block
    ; @return -> length (in m)
    (define (get-length)
      length)

    ; Function that will set the length to a new value
    ; @param new-length -> New length of the detection block (in m)
    (define (set-length! new-length)
      (set! length new-length))

    ; Function that will return the max speed (in m/s) of the detection block
    ; @return -> max-speed (in m/s)
    (define (get-max-speed)
      max-speed)

    ; Function that will set the max speed to a new value
    ; @param new-max-speed -> New max speed of the detection block (in m/s)
    (define (set-max-speed! new-max-speed)
      (set! max-speed new-max-speed))

    ; Function that will return the type of object this represents
    ; @return -> object type (string)
    (define (get-object-type)
      object-type)

    (define (dispatch message)
      (case message
        ((get-id) get-id)
        ((set-id!) set-id!)
        ((get-length) get-length)
        ((set-length!) set-length!)
        ((get-max-speed) get-max-speed)
        ((set-max-speed!) set-max-speed!)
        ((get-object-type) get-object-type)
        (else (displayln "DETECTION-BLOCK: Unknown message..."))))

    dispatch))