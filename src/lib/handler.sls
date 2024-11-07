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
