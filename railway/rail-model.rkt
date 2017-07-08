#lang racket

(provide make-rail-model)

(require "../utilities/utilities.rkt")
(require graph)

(define (make-rail-model filename)
  (let ((id-to-object-dict FALSE)
        (rail-model-graph FALSE))

    ; Based on the file create the graph linking showing connected id's with the distance between them
    (define (parse-graph-file graph-file)
      ; Initial graph creation
      (set! rail-model-graph (weighted-graph/undirected '((0 foo foo1))))
      ; Parse file (adapted the parsing from the provided simulator file, railwaymodel.rkt)
      ; 1 - Parse IDs and add them to the graph
      ; 2 - Parse edges and add them to the graph
      (let ([lines (map string-split (file->lines graph-file))])
        (for-each
         (lambda (l)
           (case (string->symbol (car l))
             [(I) (add-vertex! rail-model-graph (string->symbol (list-ref l 1)))]
             [(E) (let ([id-1 (string->symbol (list-ref l 1))]
                        [id-2 (string->symbol (list-ref l 2))]
                        [distance (string->symbol (list-ref l 3))])
                    (add-edge! rail-model-graph id-1 id-2 distance))]))
         lines))
      ; Clean up graph
      (remove-vertex! rail-model-graph 'foo)
      (remove-vertex! rail-model-graph 'foo1)
      ; Show result
      (println (get-vertices rail-model-graph))
      (println (get-edges rail-model-graph)))

    (set! rail-model-graph (parse-graph-file filename))

    (define (add-segment id length max-speed to)
      (println id length max-speed to))

    (define (dispatch message)
      (case message
        ((add-segment) add-segment)))

    dispatch))