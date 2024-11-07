;;! Common utility functions
;;!
;;! Shared helper functions used across modules:
;;! - String manipulation (split, join, contains)
;;! - Time handling (current UNIX timestamp)
;;!
;;! These are the kind of general-purpose functions that
;;! would normally come from a standard library, but we
;;! implement them ourselves to minimize dependencies.

(library (lib utils)
  (export string-split
          string-contains
          string-join
          current-seconds)
  (import (chezscheme))

  ;; Splits a string on given character into a list of substrings
  ;;
  ;; str - String to split
  ;; ch  - Character to split on
  ;;
  ;; Returns list of substrings. Empty string becomes list of one empty string.
  ;; Example: (string-split "a,b,c" #\,) -> '("a" "b" "c")
  (define (string-split str ch)
    (let loop ((chars (string->list str))
               (current '())
               (result '()))
      (cond
       ((null? chars)
        (reverse (cons (list->string (reverse current)) result)))
       ((char=? (car chars) ch)
        (loop (cdr chars)
              '()
              (cons (list->string (reverse current)) result)))
       (else
        (loop (cdr chars)
              (cons (car chars) current)
              result)))))

  ;; Joins a list of strings with a delimiter
  ;;
  ;; strings   - List of strings to join
  ;; delimiter - String to insert between elements
  ;;
  ;; Returns joined string. Empty list becomes empty string.
  ;; Example: (string-join '("a" "b" "c") ",") -> "a,b,c"
  (define (string-join strings delimiter)
  (if (null? strings)
      ""
      (let loop ((result (car strings))
                 (rest (cdr strings)))
        (if (null? rest)
            result
            (loop (string-append result delimiter (car rest))
                  (cdr rest))))))

  ;; Checks if string contains a substring
  ;;
  ;; str    - String to search in
  ;; substr - String to search for
  ;;
  ;; Returns #t if substr is found in str, #f otherwise
  ;; Example: (string-contains "hello world" "world") -> #t
  (define (string-contains str substr)
    (let ((str-len (string-length str))
          (substr-len (string-length substr)))
      (let loop ((i 0))
        (cond
          ((> (+ i substr-len) str-len) #f)
          ((string=? (substring str i (+ i substr-len)) substr) #t)
          (else (loop (+ i 1)))))))

  ;; Gets current UNIX timestamp in seconds
  ;;
  ;; Returns number of seconds since UNIX epoch
  ;; Example: (current-seconds) -> 1704587428
  (define (current-seconds)
    (time-second (date->time-utc (current-date)))))
