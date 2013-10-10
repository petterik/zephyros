#lang racket

(require json
         racket/future)

(require (planet shawnpresser/racket-unix-sockets:1:0))

(require (for-syntax racket/string))

(define-values
  (i o)
  (unix-socket-connect "/tmp/zephyros.sock"))


;; message infrastructure
(define received-messages (make-hash))
(define next-msg-id 0)
(define msg-id-sema (make-semaphore 1))
(define listeners (make-hash))

(define (get-next-msg-id)
  (semaphore-wait msg-id-sema)
  (define to-return next-msg-id)
  (set! next-msg-id (+ next-msg-id 1))
  (semaphore-post msg-id-sema)
  to-return)

(define (send-message-no-response message [receiver 'null] [args '()])
  (let* ((payload (message-payload message receiver args))
         (msg-id  (first payload)))
    (thread (lambda ()
              (displayln (jsexpr->string payload) o)
              (flush-output o)))
    msg-id))

(define (send-message message [receiver 'null] [args '()])
  (define (read-till-recv id)
    (if (hash-has-key? received-messages id)
        (hash-ref received-messages id)
        (read-till-recv id)))
  
  (read-till-recv
   (send-message-no-response message receiver args)))

(define (reader)
  (define (loop)
   (define-values (id msg+) (apply values (read-json i)))
   (hash-set! received-messages id msg+)
   (loop))
  (thread (lambda () (loop))))

(define (message-payload message [receiver 'null] [args '()])
  (append
   (list
    (get-next-msg-id) receiver message)
     args))

(define-for-syntax (get-fn-name str)
  (string->symbol
   (string-replace str "_" "-")))

(define-for-syntax (get-listener-name str)
  (string->symbol
   (format
    "on-~a"
    (string-replace str "_" "-"))))

(define-for-syntax (get-unlistener-name str)
  (string->symbol
   (format
    "unlisten-~a"
    (string-replace str "_" "-"))))

(define-syntax (protocol->response-function stx)
  (syntax-case stx ()
    ;; no args and no receiver routines
    [(_ str)
     (with-syntax ([fn-name (datum->syntax
                             stx
                             (get-fn-name (syntax->datum #'str)))])
       
       #'(define (fn-name)
           (send-message str)))]

    ;; no receiver but routine has args
    [(_ str (args ...))
     (with-syntax ([fn-name (datum->syntax
                             stx
                             (get-fn-name (syntax->datum #'str)))])
       #'(define (fn-name args ...)
           (send-message str 'null (list args ...))))]

    ;; receiver and args exist
    [(_ str receiver (args ...))
     (with-syntax ([fn-name (datum->syntax
                             stx
                             (get-fn-name (syntax->datum #'str)))])
       #'(define (fn-name receiver args ...)
           (send-message str receiver (list args ...))))]))

(define-syntax (protocol->no-response-function stx)
  (syntax-case stx ()
    ;; no args and no receiver routines
    [(_ str)
     (with-syntax ([fn-name (datum->syntax
                             stx
                             (get-fn-name (syntax->datum #'str)))])
       
       #'(define (fn-name)
           (send-message-no-response str)))]
    
    ;; no receiver but routine has args
    [(_ str (args ...))
     (with-syntax ([fn-name (datum->syntax
                             stx
                             (get-fn-name (syntax->datum #'str)))])
       #'(define (fn-name args ...)
           (send-message-no-response str 'null (list args ...))))]

    ;; receiver and args exist
    [(_ str receiver (args ...))
     (with-syntax ([fn-name (datum->syntax
                             stx
                             (get-fn-name (syntax->datum #'str)))])
       #'(define (fn-name receiver args ...)
           (send-message-no-response str receiver (list args ...))))]))

(define (poll-for-message msg-id f)
  (define last-value '*)
  (define (inner)
    (when (and (hash-has-key? received-messages msg-id)
               (not (equal? last-value
                            (hash-ref received-messages msg-id))))
      (set! last-value (hash-ref received-messages msg-id))
      (f last-value))
    (sleep 0.2)
    (inner))
  (inner))

;; listen to an event.
;; transforms window_moved -> (on-window-moved f)
(define-syntax (protocol->event-listener stx)
  (syntax-case stx ()
    [(_ str)
     (with-syntax ([fn-name (datum->syntax
                             stx (get-listener-name
                                  (syntax->datum #'str)))])
       #'(define (fn-name f)
           (define msg-id (send-message-no-response "listen" 'null '(str)))
           (hash-set! listeners
                      str
                      (thread
                       (lambda ()
                         (poll-for-message msg-id f))))))]))

(define-syntax (protocol->unlisten stx)
  (syntax-case stx ()
    [(_ str)
     (with-syntax ([fn-name (datum->syntax
                             stx (get-unlistener-name
                                  (syntax->datum #'str)))])
       #'(define (fn-name)
           (send-message-no-response "unlisten" 'null '(str))
           (kill-thread
            (hash-ref listeners str))))]))

;; top level routines that do not modify state
(protocol->response-function "clipboard_contents")
(protocol->response-function "focused_window")
(protocol->response-function "visible_windows")
(protocol->response-function "all_windows")
(protocol->response-function "main_screen")
(protocol->response-function "all_screens")
(protocol->response-function "running_apps")
(protocol->response-function "choose_from" (lst title lines-tall chars-wide))

;; top level routines that perform
;; an action (not window / screen / app related)
(protocol->no-response-function "alert" (msg duration))
(protocol->no-response-function "log" (msg))
(protocol->no-response-function "show_box" (msg))
(protocol->no-response-function "hide_box" ())
(protocol->no-response-function "update_settings" (k-v))
(protocol->no-response-function "undo")
(protocol->no-response-function "redo")

;; window routines all take a window id and args

;; window routines that return something
(protocol->response-function "title" window-id ())
(protocol->response-function "frame" window-id ())
(protocol->response-function "top_left" window-id ())
(protocol->response-function "size" window-id ())
(protocol->response-function "app" window-id ())
(protocol->response-function "screen" window-id ())
(protocol->response-function "focus_window" window-id ())
(protocol->response-function "windows_to_north" window-id ())
(protocol->response-function "windows_to_south" window-id ())
(protocol->response-function "windows_to_east" window-id ())
(protocol->response-function "windows_to_west" window-id ())
(protocol->response-function "normal_window?" window-id ())
(protocol->response-function "minimized?" window-id ())
(protocol->response-function "other_windows_on_same_screen" window-id ())
(protocol->response-function "other_windows_on_all_screens" window-id ())

;; window routines that manipulate state
(protocol->no-response-function "set_frame" window-id (x y w h))
(protocol->no-response-function "set_top_left" window-id (x y))
(protocol->no-response-function "set_size" window-id (w h))
(protocol->no-response-function "maximize" window-id ())
(protocol->no-response-function "minimize" window-id ())
(protocol->no-response-function "un_minimize" window-id ())
(protocol->no-response-function "focus_window_left" window-id ())
(protocol->no-response-function "focus_window_right" window-id ())
(protocol->no-response-function "focus_window_up" window-id ())
(protocol->no-response-function "focus_window_down" window-id ())

;; All apps take an app-id
(protocol->response-function "hidden?" app-id ())

(protocol->no-response-function "show" app-id ())
(protocol->no-response-function "hide" app-id ())
(protocol->no-response-function "kill" app-id ())
(protocol->no-response-function "kill9" app-id ())

;; Screen routines
(protocol->response-function "frame_including_dock_and_menu" screen-id ())
(protocol->response-function "frame_without_dock_or_menu" screen-id ())
(protocol->response-function "previous_screen" screen-id ())
(protocol->response-function "next_screen" screen-id ())

(protocol->no-response-function "rotate_to" screen-id (degree))

;; App routines that clash with the top level routines
(define (app/visible-windows app-id)
  (send-message "visible_windows" app-id))

(define (app/all-windows app-id)
  (send-message "all_windows" app-id))

(define (app/title app-id)
  (send-message "title" app-id))

(define (app/hidden? app-id)
  (send-message "hidden?" app-id))

;; listen to events
(protocol->event-listener "window_created")
(protocol->event-listener "window_minimized")
(protocol->event-listener "window_unminiized")
(protocol->event-listener "window_moved")
(protocol->event-listener "window_resized")
(protocol->event-listener "app_launched")
(protocol->event-listener "focus_changed")
(protocol->event-listener "app_died")
(protocol->event-listener "app_hidden")
(protocol->event-listener "app_shown")
(protocol->event-listener "screens_changed")
(protocol->event-listener "mouse_moved")
(protocol->event-listener "modifiers_changed")

(protocol->unlisten "window_created")
(protocol->unlisten "window_minimized")
(protocol->unlisten "window_unminiized")
(protocol->unlisten "window_moved")
(protocol->unlisten "window_resized")
(protocol->unlisten "app_launched")
(protocol->unlisten "focus_changed")
(protocol->unlisten "app_died")
(protocol->unlisten "app_hidden")
(protocol->unlisten "app_shown")
(protocol->unlisten "screens_changed")
(protocol->unlisten "mouse_moved")
(protocol->unlisten "modifiers_changed")

(provide (all-defined-out))

(reader)
