#lang racket

;; Client: sends request
(define-values (in out) (tcp-connect "localhost" 9883))
(write "Ping" out)
(flush-output out)
(write "S id mode" out)
(flush-output out)

(displayln (read in))

(close-input-port in)
(close-output-port out)