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


;; message infrastructure
(define received-messages (make-hash))
(define next-msg-id 0)
(define msg-id-sema (make-semaphore 1))

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

;; window infra
(define (send-window-message message window-id [args '()])
  (visible-windows)
  (send-message message window-id args))

(define (send-window-message-no-response message window-id [args '()])
  (visible-windows)
  (send-message-no-response message window-id args))

(define (window-title window-id)
  (send-window-message "title" window-id))

(define (window-set-frame window-id x y w h)
  (send-window-message-no-response "set_frame" window-id (list x y w h)))

(define (window-set-top-left window-id x y)
  (send-window-message-no-response "set_top_left" window-id (list x y)))

(define (window-set-size window-id w h)
  (send-window-message-no-response "set_size" window-id (list w h)))

(define (window-frame window-id)
  (send-window-message "frame" window-id))

(define (window-top-left window-id)
  (send-window-message "top_left" window-id))

(define (window-size window-id)
  (send-window-message "size" window-id))

(define (window-maximize window-id)
  (send-window-message-no-response "maximize" window-id))

(define (window-minimize window-id)
  (send-window-message-no-response "minimize" window-id))

(define (window-un-minimize window-id)
  (send-window-message-no-response "un_minimize" window-id))

(define (window-app window-id)
  (send-window-message "app_id" window-id))

(define (window-screen window-id)
  (send-window-message "screen_id" window-id))

(define (window-focus? window-id)
  (send-window-message "focus_window" window-id))

(define (window-focus-left window-id)
  (send-window-message-no-response "focus_window_left" window-id))

(define (window-focus-right window-id)
  (send-window-message-no-response "focus_window_right" window-id))

(define (window-focus-up window-id)
  (send-window-message-no-response "focus_window_up" window-id))

(define (window-focus-down window-id)
  (send-window-message-no-response "focus_window_down" window-id))

(define (windows-to-north window-id)
  (send-window-message "windows_to_north" window-id))

(define (windows-to-south window-id)
  (send-window-message "windows_to_south" window-id))

(define (windows-to-east window-id)
  (send-window-message "windows_to_east" window-id))

(define (windows-to-west window-id)
  (send-window-message "windows_to_west" window-id))

(define (normal-window? window-id)
  (send-window-message "normal_window?" window-id))

(define (minimized? window-id)
  (send-window-message "minimized?" window-id))

(define (other-windows-on-same-screen window-id)
  (send-window-message "other_windows_on_same_screen" window-id))

(define (other-windows-on-all-screens window-id)
  (send-window-message "other_windows_on_all_screens" window-id))

(reader)
