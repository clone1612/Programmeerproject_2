#lang racket

(provide make-rail-model)

(require "../utilities/utilities.rkt")
(require "basic_segment.rkt")
(require "detection_block.rkt")
(require "switch.rkt")
(require graph)

(define (make-rail-model filename)
  (let ((id-to-object-hash FALSE)
        (rail-model-graph FALSE))

    ; Based on the file create the graph linking showing connected id's with the distance between them
    (define (parse-graph-file graph-file)
      ; Initial graph creation
      (set! rail-model-graph (weighted-graph/undirected '((0 foo foo1))))
      ; Initial hash table creation
      (set! id-to-object-hash (make-hash))
      ; Parse file (adapted the parsing from the provided simulator file, railwaymodel.rkt)
      ; 1 - Parse IDs and add them to the graph
      ; 2 - Parse edges and add them to the graph
      ; 3 - Parse the actual railway elements (objects like locomotive, block, switch, segment)
      (let ([lines (map string-split (file->lines graph-file))])
        (for-each
         (lambda (l)
           (case (string->symbol (car l))
             [(I) (add-vertex! rail-model-graph (string->symbol (list-ref l 1)))]
             [(E) (let ([id-1 (string->symbol (list-ref l 1))]
                        [id-2 (string->symbol (list-ref l 2))]
                        [distance (string->symbol (list-ref l 3))])
                    (add-edge! rail-model-graph id-1 id-2 distance))]
             [(OBS) (let ([id (string->symbol (list-ref l 1))]
                          [max-speed (string->symbol (list-ref l 2))])
                      (hash-set! id-to-object-hash id (make-basic-segment id 0 max-speed)))]
             [(ODB) (let ([id (string->symbol (list-ref l 1))]
                          [max-speed (string->symbol (list-ref l 2))])
                      (hash-set! id-to-object-hash id (make-detection-block id 0 max-speed)))]
             [(OS) (let ([id (string->symbol (list-ref l 1))]
                         [start (string->symbol (list-ref l 2))]
                         [d1 (cons (string->symbol (list-ref l 3)) 0)]
                         [d2 (cons (string->symbol (list-ref l 4)) 0)])
                     (if (= (length l) 5)
                         (hash-set! id-to-object-hash id (make-switch id start d1 d2))
                         (hash-set! id-to-object-hash id (make-switch id start d1 d2 (cons (string->symbol (list-ref l 5)) 0)))))]))
         lines))
      ; Clean up graph
      (remove-vertex! rail-model-graph 'foo)
      (remove-vertex! rail-model-graph 'foo1)
      ; Show result
      (println (get-vertices rail-model-graph))
      (println (get-edges rail-model-graph))
      (println (hash-keys id-to-object-hash)))

    ; Function that will save our model
    (define (save-graph-file filename)
      ; Helper function to write all IDs
      ; Helper function to write all edges
      ; Helper function to write all objects
      "TODO")

    (set! rail-model-graph (parse-graph-file filename))

    ; Function that will return the object corresponding with a certain id
    ; @param id -> id of the object we want to return
    ; @return -> object with the given id
    (define (get-object id)
      (hash-ref id-to-object-hash id))

    ; Helper function that will handle the addition of new elements to the model
    (define (add-to-model id to length element)
      ; Add vertex to the graph
      (add-vertex! rail-model-graph id)
      ; Add edge to the graph
      (add-edge! rail-model-graph id to length)
      ; Add actual element to the model
      (hash-set! id-to-object-hash id element))

    (define (add-segment id length max-speed to)
      (add-to-model id to (make-basic-segment id length max-speed)))

    (define (add-detection-block id length max-speed to)
      (add-to-model id to (make-detection-block id length max-speed)))

    (define (dispatch message)
      (case message
        ((get-object) get-object)
        ((add-segment) add-segment)
        ((add-detection-block) add-detection-block)))

    dispatch))