(library (lib http)
  (export parse-headers
          get-path
          get-method
          get-cookies
          get-post-data
          hx-request?
          make-cookie)
  (import (chezscheme)
          (lib utils))

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

  (define (get-cookies headers)
    (let ((cookie-header (find-header headers "Cookie:")))
      (display "Cookie header: ") (display cookie-header) (newline)
      (if cookie-header
          (let ((cookies (parse-cookies (substring cookie-header 8 (string-length cookie-header)))))
            (display "Parsed cookies: ") (display cookies) (newline)
            cookies)
          '())))

  (define (parse-cookies str)
    (map (lambda (pair)
           (let ((parts (string-split pair #\=)))
             (cons (string-trim (car parts))
                   (string-trim (cadr parts)))))
         (string-split str #\;)))

  (define (make-cookie name value)
    (string-append "Set-Cookie: " name "=" value))

  (define (get-post-data headers data)
  (let* ((content-length-header (find-header headers "Content-Length:"))
         (content-length (and content-length-header
                            (string->number (substring content-length-header
                                                     16
                                                     (string-length content-length-header)))))
         (body-start (find-empty-line data)))
    (display "Content-Length: ") (display content-length) (newline)
    (if (and content-length body-start)
        (let ((body (substring data
                              (+ body-start 1)
                              (string-length data))))
          (display "Body: ") (display body) (newline)
          (let ((parsed (parse-urlencoded body)))
            (display "Parsed: ") (display parsed) (newline)
            parsed))
        '())))

  (define (parse-urlencoded data)
    (map (lambda (pair)
           (let ((parts (string-split pair #\=)))
             (cons (uri-decode (car parts))
                  (uri-decode (cadr parts)))))
         (string-split data #\&)))

  (define (hx-request? headers)
    (find-header headers "HX-Request:"))

  (define (find-header headers prefix)
    (let ((header (find (lambda (h)
                         (string-prefix? prefix h))
                       headers)))
      (and header
           (string-trim header))))

  (define (find-empty-line str)
    (let loop ((i 0))
      (cond
        ((>= (+ i 2) (string-length str)) #f)
        ((and (char=? (string-ref str i) #\return)
              (char=? (string-ref str (+ i 1)) #\newline)
              (char=? (string-ref str (+ i 2)) #\return)
              (char=? (string-ref str (+ i 3)) #\newline))
         (+ i 3))
        (else (loop (+ i 1))))))

  ;; String utils specific to HTTP/URI handling
  (define (string-trim str)
    (let* ((start (string-skip str char-whitespace?))
           (end (string-skip-right str char-whitespace?)))
      (if (or (not start) (not end))
          ""
          (substring str start (+ end 1)))))

  (define (string-prefix? prefix str)
    (and (>= (string-length str) (string-length prefix))
         (string=? (substring str 0 (string-length prefix)) prefix)))

  (define (string-skip str pred)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((not (pred (string-ref str i))) i)
        (else (loop (+ i 1))))))

  (define (string-skip-right str pred)
    (let loop ((i (- (string-length str) 1)))
      (cond
        ((< i 0) #f)
        ((not (pred (string-ref str i))) i)
        (else (loop (- i 1))))))

  (define (uri-decode str)
    (let ((len (string-length str)))
      (let loop ((i 0) (result '()))
        (if (>= i len)
            (list->string (reverse result))
            (let ((c (string-ref str i)))
              (cond
                ((char=? c #\+)
                 (loop (+ i 1) (cons #\space result)))
                ((char=? c #\%)
                 (loop (+ i 3)
                       (cons (integer->char
                              (string->number
                                (substring str (+ i 1) (+ i 3))
                                16))
                             result)))
                (else
                 (loop (+ i 1) (cons c result))))))))))
