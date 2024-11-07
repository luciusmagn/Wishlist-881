(library (lib router)
  (export find-handler)
  (import (chezscheme)
          (lib handler))

  (define (find-handler handlers path)
    (find (lambda (h)
            (string=? (handler-path h) path))
          handlers)))
