#lang racket

(require json
         racket/future
         racket/place)

(require (planet shawnpresser/racket-unix-sockets:1:0))

(provide clipboard-contents
         focused-window
         visible-windows
         all-windows
         main-screen
         all-screens
         running-apps)


(define-values
  (i o)
  (unix-socket-connect "/tmp/zephyros.sock"))

(define received-messages (make-hash))

(define next-msg-id 0)

(define (get-next-msg-id)
  (let ((to-return next-msg-id))
    (set! next-msg-id (+ next-msg-id 1))
    to-return))

(define (send-message message)
  (define (read-till-recv id)
    (if (hash-has-key? received-messages id)
        (hash-ref received-messages id)
        (read-till-recv id)))
  (thread (lambda ()
            (displayln (message-str->json-str message) o)
            (flush-output o))))

(define (reader)
  (define (loop)
   (define-values (id msg+) (apply values (read-json i)))
   (hash-set! received-messages id msg+)
   (loop))
  (thread (lambda () (loop))))

(define (message-str->json-str message [args '()])
  (jsexpr->string (append
                   (list
                    (get-next-msg-id)
                    0
                    message)
                   args)))

(define (read-message)
  '*)

;; routines that just return values and don't change
;; state
(define (clipboard-contents)
  (send-message "clipboard_contents")
  (read-message))

(define (focused-window)
  (send-message "focused_window")
  (read-message))

(define (visible-windows)
  (send-message "visible_windows")
  (read-message))

(define (all-windows)
  (send-message "all_windows")
  (read-message))

(define (main-screen)
  (send-message "main_screen")
  (read-message))

(define (all-screens)
  (send-message "all_screens")
  (read-message))

(define (running-apps)
  (send-message "running_apps")
  (read-message))

