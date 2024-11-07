;;! HTTP request handler infrastructure
;;!
;;! Provides foundation for routing and handling HTTP requests:
;;! - Handler record type for storing endpoint info
;;! - Registration system for both regular HTTP and HTMX handlers
;;! - Path pattern matching (e.g., "/items/:id")
;;!
;;! The distinction between HTTP and HTMX handlers allows us to
;;! serve both full pages and partial content for dynamic updates.
;;!
;;! Each handler contains two hashtables:
;;! - plain-handlers for regular HTTP requests
;;! - hx-handlers for HTMX-specific responses

(library (lib handler)
  (export make-empty-handler
          handler-path
          handler-hx-handlers
          handler-plain-handlers)
  (import (chezscheme))

  ;; Record type for HTTP request handlers
  ;;
  ;; Fields:
  ;; path           - String URL pattern (e.g., "/items/:id")
  ;; hx-handlers    - Hashtable mapping methods to HTMX handlers
  ;; plain-handlers - Hashtable mapping methods to regular handlers
  ;;
  ;; Notes:
  ;; - Methods are symbols (get, post, etc.)
  ;; - Handlers are procedures taking (client headers data params)
  ;; - HTMX handlers return partial HTML
  ;; - Plain handlers return full pages
  (define-record-type handler
    (fields path
            hx-handlers
            plain-handlers))

  ;; Creates new empty handler for given path
  ;;
  ;; path - String URL pattern to match
  ;;
  ;; Returns: Handler record with empty hashtables
  ;;
  ;; Example:
  ;; (make-empty-handler "/items/:id")
  ;;
  ;; Notes:
  ;; - Uses eq? hashtables (symbols as keys)
  ;; - Ready for registering GET/POST/etc handlers
  (define (make-empty-handler path)
    (make-handler path
                 (make-eq-hashtable)
                 (make-eq-hashtable))))
