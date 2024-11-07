(library (lib http)
  (export parse-headers
          get-path
          get-method
          hx-request?)
  (import (chezscheme))

  (define (read-headers port)
    (let loop ((lines '()))
      (let ((line (read-header-line port)))
        (if (string=? line "")
            (reverse lines)
            (loop (cons line lines))))))

  (define (read-header-line port)
    (let loop ((chars '()))
      (let ((c (read-char port)))
        (cond
          ((eof-object? c)
           (list->string (reverse chars)))
          ((char=? c #\return)
           (let ((next (read-char port)))
             (if (char=? next #\newline)
                 (list->string (reverse chars))
                 (loop (cons* next c chars)))))
          (else
           (loop (cons c chars)))))))

  (define (parse-headers data)
    (let ((port (open-input-string data)))
      (read-headers port)))

  (define (get-path headers)
    (let* ((request-line (car headers))
           (parts (string-split request-line #\space)))
      (cadr parts)))

  (define (get-method headers)
    (let* ((request-line (car headers))
           (parts (string-split request-line #\space)))
      (string->symbol (string-downcase (car parts)))))

  (define (hx-request? headers)
    (exists (lambda (line)
              (string-contains line "HX-Request:"))
            headers))

  ;; Utils
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

  (define (string-contains str substr)
    (let ((str-len (string-length str))
          (substr-len (string-length substr)))
      (let loop ((i 0))
        (cond
          ((> (+ i substr-len) str-len) #f)
          ((string=? (substring str i (+ i substr-len)) substr) #t)
          (else (loop (+ i 1))))))))
