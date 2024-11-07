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
;;!
;;! The server distinguishes between two types of handlers:
;;! - Plain handlers (full HTML pages)
;;! - HTMX handlers (partial HTML for dynamic updates)

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

  ;; Registers an HTMX-specific handler
  ;;
  ;; handler - Handler record to register with
  ;; method  - Symbol HTTP method (get, post, delete, etc.)
  ;; proc    - Procedure taking (client headers data params)
  ;;
  ;; Side effects:
  ;; - Adds procedure to handler's HTMX hashtable
  ;;
  ;; Notes:
  ;; - Overwrites existing handler for same method
  ;; - proc should send response using send-response
  (define (register-hx-handler! handler method proc)
    (hashtable-set! (handler-hx-handlers handler) method proc))

  ;; Registers a regular HTTP handler
  ;;
  ;; handler - Handler record to register with
  ;; method  - Symbol HTTP method (get, post, delete, etc.)
  ;; proc    - Procedure taking (client headers data params)
  ;;
  ;; Side effects:
  ;; - Adds procedure to handler's plain hashtable
  ;;
  ;; Notes:
  ;; - Overwrites existing handler for same method
  ;; - proc should send response using send-response
  ;; - Used for full page loads
  (define (register-plain-handler! handler method proc)
    (hashtable-set! (handler-plain-handlers handler) method proc))

  ;; Handles incoming HTTP request
  ;;
  ;; client   - Socket file descriptor for client
  ;; headers  - List of header strings
  ;; data     - Raw request data string
  ;; path     - String URL path
  ;; method   - Symbol HTTP method
  ;; is-hx    - Boolean indicating if request is from HTMX
  ;; handlers - List of handler records
  ;;
  ;; Side effects:
  ;; - Logs request info to stdout
  ;; - Sends response to client
  ;; - Closes client socket
  ;;
  ;; Notes:
  ;; - Automatically sends 404 if no handler found
  ;; - Automatically sends 404 if method not supported
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

  ;; Starts HTTP server on specified port
  ;;
  ;; port     - Integer port number to listen on
  ;; handlers - List of handler records for routing
  ;;
  ;; Side effects:
  ;; - Creates server socket
  ;; - Prints startup message
  ;; - Enters infinite accept loop
  ;;
  ;; Notes:
  ;; - Never returns (runs until process ends)
  ;; - Each request handled in same thread
  ;; - Automatically detects HTMX requests
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

  ;; Sends HTTP response to client
  ;;
  ;; client  - Socket file descriptor for client
  ;; code    - Integer HTTP status code
  ;; body    - String response body
  ;; cookies - Optional list of Set-Cookie header strings
  ;;
  ;; Side effects:
  ;; - Sends response headers and body
  ;; - Closes client socket
  ;;
  ;; Notes:
  ;; - Always sends HTTP/1.1
  ;; - Automatically adds required CRLFs
  ;; - Handles cookie headers if provided
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

  ;; Sends 404 Not Found response
  ;;
  ;; client - Socket file descriptor for client
  ;;
  ;; Side effects:
  ;; - Sends response
  ;; - Closes client socket
  (define (send-404 client)
    (send-response client 404 "Not Found")))
