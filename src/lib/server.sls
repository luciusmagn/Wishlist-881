(library (lib server)
  (export register-hx-handler!
          register-plain-handler!
          run-server
          send-response)
  (import (chezscheme)
          (lib socket)
          (lib http)
          (lib router)
          (lib handler))

  (define (register-hx-handler! handler method proc)
    (hashtable-set! (handler-hx-handlers handler) method proc))

  (define (register-plain-handler! handler method proc)
    (hashtable-set! (handler-plain-handlers handler) method proc))

  (define (handle-request client path method is-hx handlers)
    (let ((handler (find-handler handlers path)))
      (if handler
          (let* ((table (if is-hx
                           (handler-hx-handlers handler)
                           (handler-plain-handlers handler)))
                 (proc (hashtable-ref table method #f)))
            (if proc
                (proc client)
                (send-404 client)))
          (send-404 client))))

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
          (handle-request client path method is-hx handlers))
        (loop))))

  (define (send-response client code body)
    (socket-send client
                 (string-append "HTTP/1.1 "
                              (number->string code)
                              "\r\n\r\n"
                              body))
    (close-socket client))

  (define (send-404 client)
    (send-response client 404 "Not Found")))
