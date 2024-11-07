;;! Core HTTP server implementation
;;!
;;! Ties together all the HTTP components:
;;! - Server lifecycle management
;;! - Request acceptance and parsing
;;! - Handler dispatch
;;! - Response sending
;;! - Error handling
;;!
;;! This is where the main server loop lives, accepting connections
;;! and delegating them to appropriate handlers. It's also responsible
;;! for detecting HTMX requests and routing them differently.

(library (lib server)
  (export register-hx-handler!
          register-plain-handler!
          run-server
          send-response)
  (import (chezscheme)
          (lib socket)
          (lib http)
          (lib router)
          (lib handler)
          (lib utils))

  (define (register-hx-handler! handler method proc)
    (hashtable-set! (handler-hx-handlers handler) method proc))

  (define (register-plain-handler! handler method proc)
    (hashtable-set! (handler-plain-handlers handler) method proc))

  (define (handle-request client headers data path method is-hx handlers)
    (display "Request: ") (display method) (display " ") (display path) (newline)
    (let-values (((handler params) (find-handler handlers path)))
      (if handler
          (begin
            (display "Handler found with params: ") (display params) (newline)
            (let* ((table (if is-hx
                              (handler-hx-handlers handler)
                              (handler-plain-handlers handler)))
                   (proc (hashtable-ref table method #f)))
              (if proc
                  (proc client headers data params)  ; Pass params to handler
                  (begin
                    (display "Method not found in handler") (newline)
                    (send-404 client)))))
          (begin
            (display "No handler found for path") (newline)
            (send-404 client)))))

  (define (run-server port handlers)
    (let ((sock (make-server-socket port)))
      (display "Server running on port ") (display port) (newline)
      (let loop ()
        (let* ((client (accept-connection sock))
               (data (socket-recv client))
               (headers (parse-headers data))
               (path (get-path headers))
               (method (get-method headers))
               (is-hx (hx-request? headers)))
          (handle-request client headers data path method is-hx handlers))
        (loop))))

  (define (send-response client code body . cookies)
    (socket-send client
                 (string-append "HTTP/1.1 "
                              (number->string code)
                              "\r\n"
                              (if (null? cookies)
                                  ""
                                  (string-append
                                   (string-join cookies "\r\n")
                                   "\r\n"))
                              "\r\n"
                              body))
    (close-socket client))

  (define (send-404 client)
    (send-response client 404 "Not Found")))
