(library (lib utils)
  (export string-split
          string-contains
          string-join
          current-seconds)
  (import (chezscheme))

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


  (define (string-join strings delimiter)
  (if (null? strings)
      ""
      (let loop ((result (car strings))
                 (rest (cdr strings)))
        (if (null? rest)
            result
            (loop (string-append result delimiter (car rest))
                  (cdr rest))))))

  (define (string-contains str substr)
    (let ((str-len (string-length str))
          (substr-len (string-length substr)))
      (let loop ((i 0))
        (cond
          ((> (+ i substr-len) str-len) #f)
          ((string=? (substring str i (+ i substr-len)) substr) #t)
          (else (loop (+ i 1)))))))

  (define (current-seconds)
    (time-second (date->time-utc (current-date)))))
