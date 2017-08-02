#lang racket

(require "../entities/infrabel.rkt")
(require "../entities/nmbs.rkt")
(require "../utilities/utilities.rkt")

(define i (infrabel))
(define n (nmbs))

(send i 'start)
(send n 'start)

;(send n 'send-command "set-speed! 1 0.1")

(define (stop)
  (send i 'stop)
  (send n 'stop))