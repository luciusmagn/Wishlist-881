;;! URL routing and parameter extraction
;;!
;;! Handles mapping of URLs to handler functions:
;;! - Pattern matching for URLs (e.g., "/items/:id/edit")
;;! - Parameter extraction from URLs into alists
;;! - Handler lookup and validation
;;!
;;! The router is what allows us to write paths with placeholders
;;! and then access those values in our handlers. It's the bridge
;;! between URLs and application logic.

(library (lib router)
  (export find-handler)
  (import (chezscheme)
          (lib utils)
          (lib handler))

  (define (path->pattern path)
    (map (lambda (part)
           (if (and (> (string-length part) 0)
                   (char=? (string-ref part 0) #\:))
               (string->symbol (substring part 1 (string-length part)))
               part))
         (string-split path #\/)))

  (define (match-pattern pattern-parts path-parts)
    (if (not (= (length pattern-parts) (length path-parts)))
        (cons #f '())  ; Return empty params if lengths don't match
        (let loop ((pat pattern-parts)
                   (path path-parts)
                   (params '()))
          (cond
           ((null? pat) (cons #t params))
           ((symbol? (car pat))
            (loop (cdr pat)
                  (cdr path)
                  (cons (cons (car pat) (car path)) params)))
           ((string=? (car pat) (car path))
            (loop (cdr pat) (cdr path) params))
           (else (cons #f params))))))

  (define (find-handler handlers path)
    (let ((path-parts (string-split path #\/)))
      (let loop ((hs handlers))
        (if (null? hs)
            (values #f '())  ; Use values to return multiple values
            (let* ((h (car hs))
                   (pattern (path->pattern (handler-path h)))
                   (result (match-pattern pattern path-parts)))
              (if (car result)
                  (values h (cdr result))  ; Use values here too
                  (loop (cdr hs)))))))))
