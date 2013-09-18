#lang racket

(require json
         racket/future)

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

(define (send-message-no-response message [args '()])
  (let* ((payload (message-payload message args))
         (msg-id  (first payload)))
    (thread (lambda ()
              (displayln (jsexpr->string payload) o)
              (flush-output o)))
    msg-id))

(define (send-message message [args '()])
  (define (read-till-recv id)
    (if (hash-has-key? received-messages id)
        (hash-ref received-messages id)
        (read-till-recv id)))
  
  (read-till-recv
   (send-message-no-response message args)))

(define (reader)
  (define (loop)
   (define-values (id msg+) (apply values (read-json i)))
   (hash-set! received-messages id msg+)
   (loop))
  (thread (lambda () (loop))))

(define (message-payload message [args '()])
  (append
   (list
    (get-next-msg-id) 0 message)
     args))

;; routines that just return values and don't change
;; state
(define (clipboard-contents)
  (send-message "clipboard_contents"))

(define (focused-window)
  (send-message "focused_window"))

(define (visible-windows)
  (send-message "visible_windows"))

(define (all-windows)
  (send-message "all_windows"))

(define (main-screen)
  (send-message "main_screen"))

(define (all-screens)
  (send-message "all_screens"))

(define (running-apps)
  (send-message "running_apps"))

;; top level routines that perform
;; an action (not window / screen / app related)
(define (alert msg duration)
  (send-message-no-response
   "alert" (list msg duration)))

(define (log msg)
  (send-message-no-response
   "log" (list msg)))

(define (show-box msg)
  (send-message-no-response
   "show_box" (list msg)))

(define (hide-box)
  (send-message-no-response
   "hide_box"))

(define (choose-from lst title lines_tall chars_wide)
  (send-message
   "choose_from"
   (list lst title lines_tall chars_wide)))

(define (update-settings k-v)
  (send-message-no-response
   "update_settings"
   (list k-v)))

(define (undo)
  (send-message "undo"))

(define (redo)
  (send-message "redo"))

(reader)
