;;! HTTP request handler infrastructure
;;!
;;! Provides foundation for routing and handling HTTP requests:
;;! - Handler record type for storing endpoint info
;;! - Registration system for both regular HTTP and HTMX handlers
;;! - Path pattern matching (e.g., "/items/:id")
;;!
;;! The distinction between HTTP and HTMX handlers allows us to
;;! serve both full pages and partial content for dynamic updates.

(library (lib handler)
  (export make-empty-handler
          handler-path
          handler-hx-handlers
          handler-plain-handlers)
  (import (chezscheme))

  (define-record-type handler
    (fields path
            hx-handlers
            plain-handlers))

  (define (make-empty-handler path)
    (make-handler path
                 (make-eq-hashtable)
                 (make-eq-hashtable))))
