#lang racket

(require racket/logging)
(require racket/date)

(provide make-a-logger)

(define (make-a-logger filename)
  (let ([file filename])
    
    (define (write-message-to-file message)
      (let* ([current-time-date (date->string (current-date) #t)]
             [log-string (string-append current-time-date ": " message)])
        (call-with-output-file file
          (lambda (out)
            (with-logging-to-port out
              (lambda ()
                (log-info log-string))
              'info))
          #:exists 'append)))

    ; Function that will allow a message to be logged to a file
    ; @param message -> message to log
    (define (log-to-file message)
      (write-message-to-file message))

    (define (dispatch message)
      (case message
        ((log-to-file) log-to-file)))

    dispatch))