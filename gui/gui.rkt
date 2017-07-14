#lang racket

(require racket/gui)

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Example"]))
 
; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))
 
; Make a button in the frame
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])

; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    ; Call the superclass init, passing on all init args
    (super-new)))
 
; Make a canvas that handles events in the frame
(new my-canvas% [parent frame])

(new button% [parent frame]
             [label "Pause"]
             [callback (lambda (button event) (sleep 5))])

(define tab (new tab-panel%
     [choices (list "Manage" "View Log")]
     [parent frame]
     [min-width 250]
     [min-height 350]
     [callback (lambda (tab event)
                 (send msg set-label (send tab get-item-label (send tab get-selection))))]))

(define panel (new horizontal-panel% [parent tab]))
(new button% [parent panel]
             [label "Left"]
             [callback (lambda (button event)
                         (send msg set-label "Left click"))])
(new button% [parent panel]
             [label "Right"]
             [callback (lambda (button event)
                         (send msg set-label "Right click"))])

; Draw all nodes
(define (draw-nodes can dc)
  ; Get the list of nodes, for now just hardcoded
  (define node-list (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l))
  (define node-edges (list '('a 'c) '('e 'l)))
  (define-values (width height) (send dc get-size))
  (let ([x-counter 1]
        [y-counter 1]
        [node-x-space (/ width 8)]
        [node-y-space (/ height 8)])
    (map
     (lambda (node)
       (send dc draw-ellipse (+ (- 10 (/ 15 2)) (* node-x-space x-counter)) (+ (- 10 (/ 15 2)) (* node-y-space y-counter)) 15 15)
       (if (> x-counter 8)
           (begin
             (set! x-counter 1)
             (set! y-counter (+ y-counter 1)))
           (set! x-counter (+ x-counter 1))))
     node-list)))

(define canvas (new canvas%
                    [parent panel]
                    [paint-callback
                     draw-nodes]))
                     ;(lambda (can dc)
                      ; (define-values (x y) (send dc get-size))
                       ; Draw a railtrack
                       ;(send dc draw-line 10 10 (- x 10) (- y 10))
                       ; Draw a node
                       ;(send dc draw-ellipse (- 10 (/ 15 2)) (- 10 (/ 15 2)) 15 15))]))

; Show the frame by calling its show method
(send frame show #t)