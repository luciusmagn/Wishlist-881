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
;;!
;;! Example:
;;! Pattern "/items/:id" matches "/items/123" and extracts
;;! ((id . "123")) as parameters.

(library (lib router)
  (export find-handler)
  (import (chezscheme)
          (lib utils)
          (lib handler))

  ;; Converts URL pattern into list of path components and symbols
  ;;
  ;; path - String URL pattern (e.g., "/items/:id/edit")
  ;;
  ;; Returns: List where each element is either:
  ;; - String for static path component
  ;; - Symbol for parameter (stripped of ":")
  ;;
  ;; Example:
  ;; (path->pattern "/items/:id/edit")
  ;; -> '("items" id "edit")
  ;;
  ;; Notes:
  ;; - Empty path components are preserved
  ;; - Parameters must start with ":"
  (define (path->pattern path)
    (map (lambda (part)
           (if (and (> (string-length part) 0)
                   (char=? (string-ref part 0) #\:))
               (string->symbol (substring part 1 (string-length part)))
               part))
         (string-split path #\/)))

  ;; Matches URL path against pattern and extracts parameters
  ;;
  ;; pattern-parts - List from path->pattern
  ;; path-parts   - List of actual URL components
  ;;
  ;; Returns: Pair where:
  ;; - car is #t if match successful, #f otherwise
  ;; - cdr is alist of extracted parameters
  ;;
  ;; Example:
  ;; (match-pattern '("items" id) '("items" "123"))
  ;; -> (#t . ((id . "123")))
  ;;
  ;; Notes:
  ;; - Path and pattern must have same number of parts
  ;; - Static parts must match exactly
  ;; - Symbol parts capture corresponding path component
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

  ;; Finds matching handler for URL path
  ;;
  ;; handlers - List of handler records
  ;; path     - String URL path to match
  ;;
  ;; Returns: Two values via values:
  ;; 1. Matching handler record or #f
  ;; 2. Alist of extracted parameters
  ;;
  ;; Example:
  ;; (find-handler handlers "/items/123")
  ;; -> <handler-record>, ((id . "123"))
  ;;
  ;; Notes:
  ;; - Returns first matching handler
  ;; - Returns #f, '() if no handler matches
  ;; - Handlers searched in order provided
  (define (find-handler handlers path)
    (let ((path-parts (string-split path #\/)))
      (let loop ((hs handlers))
        (if (null? hs)
            (values #f '())
            (let* ((h (car hs))
                   (pattern (path->pattern (handler-path h)))
                   (result (match-pattern pattern path-parts)))
              (if (car result)
                  (values h (cdr result))
                  (loop (cdr hs)))))))))
