;;! HTTP protocol implementation
;;!
;;! Complete handling of HTTP communication:
;;! - Request parsing (method, path, headers)
;;! - Cookie management (session handling)
;;! - Form data parsing (for POST requests)
;;! - Response generation with proper headers
;;!
;;! This is a minimal HTTP/1.1 implementation that focuses on
;;! the features we actually need, built directly on top of
;;! the socket layer without external dependencies.
;;!
;;! Note: This implementation assumes well-formed requests and
;;! doesn't handle all edge cases that a production server would need.

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

  ;; Reads HTTP headers from port until empty line
  ;;
  ;; port - Input port to read from
  ;;
  ;; Returns: List of header strings
  ;;
  ;; Notes:
  ;; - Handles both LF and CRLF line endings
  ;; - Stops at first empty line (end of headers)
  ;; - Preserves original header case
  (define (read-headers port)
    (let loop ((lines '()))
      (let ((line (read-header-line port)))
        (if (string=? line "")
            (reverse lines)
            (loop (cons line lines))))))

  ;; Reads single header line handling CRLF
  ;;
  ;; port - Input port to read from
  ;;
  ;; Returns: String containing header line without line ending
  ;;
  ;; Notes:
  ;; - Handles both \n and \r\n endings
  ;; - Returns empty string at EOF
  ;; - Preserves original header text
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

  ;; Parses raw HTTP request into headers
  ;;
  ;; data - String containing raw HTTP request
  ;;
  ;; Returns: List of header strings including request line
  ;;
  ;; Notes:
  ;; - First line is always the request line
  ;; - Maintains original header order
  (define (parse-headers data)
    (let ((port (open-input-string data)))
      (read-headers port)))

  ;; Extracts path from HTTP request
  ;;
  ;; headers - List of header strings
  ;;
  ;; Returns: String containing request path
  ;;
  ;; Example:
  ;; "GET /path HTTP/1.1" -> "/path"
  ;;
  ;; Notes:
  ;; - Assumes first header is request line
  ;; - Doesn't decode URL encoding yet
  (define (get-path headers)
    (let* ((request-line (car headers))
           (parts (string-split request-line #\space)))
      (cadr parts)))

  ;; Extracts HTTP method from request
  ;;
  ;; headers - List of header strings
  ;;
  ;; Returns: Symbol representing method (get, post, etc.)
  ;;
  ;; Example:
  ;; "GET /path HTTP/1.1" -> 'get
  ;;
  ;; Notes:
  ;; - Converts to lowercase symbol
  ;; - Assumes first header is request line
  (define (get-method headers)
    (let* ((request-line (car headers))
           (parts (string-split request-line #\space)))
      (string->symbol (string-downcase (car parts)))))

  ;; Extracts cookies from request headers
  ;;
  ;; headers - List of header strings
  ;;
  ;; Returns: Alist of cookie name-value pairs
  ;;
  ;; Example:
  ;; Cookie: session=abc; user=123
  ;; -> ((session . "abc") (user . "123"))
  ;;
  ;; Notes:
  ;; - Returns empty list if no Cookie header
  ;; - Handles multiple cookies in one header
  ;; - Trims whitespace from names and values
  (define (get-cookies headers)
    (let ((cookie-header (find-header headers "Cookie:")))
      (display "Cookie header: ") (display cookie-header) (newline)
      (if cookie-header
          (let ((cookies (parse-cookies (substring cookie-header 8 (string-length cookie-header)))))
            (display "Parsed cookies: ") (display cookies) (newline)
            cookies)
          '())))

  ;; Parses Cookie header value into alist
  ;;
  ;; str - String containing cookie data
  ;;
  ;; Returns: Alist of cookie name-value pairs
  ;;
  ;; Example:
  ;; "session=abc; user=123"
  ;; -> ((session . "abc") (user . "123"))
  (define (parse-cookies str)
    (map (lambda (pair)
           (let ((parts (string-split pair #\=)))
             (cons (string-trim (car parts))
                   (string-trim (cadr parts)))))
         (string-split str #\;)))

  ;; Creates Set-Cookie header string
  ;;
  ;; name  - String cookie name
  ;; value - String cookie value
  ;;
  ;; Returns: String containing full Set-Cookie header
  ;;
  ;; Example:
  ;; (make-cookie "session" "abc")
  ;; -> "Set-Cookie: session=abc"
  (define (make-cookie name value)
    (string-append "Set-Cookie: " name "=" value))

  ;; Extracts POST data from request
  ;;
  ;; headers - List of header strings
  ;; data    - Raw request data string
  ;;
  ;; Returns: Alist of form parameters
  ;;
  ;; Example:
  ;; "name=value&other=123"
  ;; -> ((name . "value") (other . "123"))
  ;;
  ;; Notes:
  ;; - Handles URL-encoded form data
  ;; - Returns empty list if no data
  ;; - Requires Content-Length header
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

  ;; Parses URL-encoded form data
  ;;
  ;; data - String containing form data
  ;;
  ;; Returns: Alist of decoded name-value pairs
  ;;
  ;; Example:
  ;; "name=hello+world&id=123"
  ;; -> ((name . "hello world") (id . "123"))
  ;;
  ;; Notes:
  ;; - Handles + as space
  ;; - Handles %XX hex encoding
  ;; - Splits on & character
  (define (parse-urlencoded data)
    (map (lambda (pair)
           (let ((parts (string-split pair #\=)))
             (cons (uri-decode (car parts))
                  (uri-decode (cadr parts)))))
         (string-split data #\&)))

  ;; Checks if request is from HTMX
  ;;
  ;; headers - List of header strings
  ;;
  ;; Returns: String header value if HTMX request, #f otherwise
  ;;
  ;; Notes:
  ;; - Looks for HX-Request header
  ;; - Used to determine if request needs partial response
  (define (hx-request? headers)
    (find-header headers "HX-Request:"))

  ;; Finds header by prefix in header list
  ;;
  ;; headers - List of header strings
  ;; prefix  - String to match at start of header
  ;;
  ;; Returns: Full header string if found, #f otherwise
  ;;
  ;; Example:
  ;; (find-header headers "Content-Type:")
  ;; -> "Content-Type: text/html"
  ;;
  ;; Notes:
  ;; - Case sensitive matching
  ;; - Returns trimmed header string
  (define (find-header headers prefix)
    (let ((header (find (lambda (h)
                         (string-prefix? prefix h))
                       headers)))
      (and header
           (string-trim header))))

  ;; Finds empty line marking end of headers
  ;;
  ;; str - Raw HTTP request string
  ;;
  ;; Returns: Index after empty line or #f if not found
  ;;
  ;; Notes:
  ;; - Looks for \r\n\r\n sequence
  ;; - Returns position after sequence
  ;; - Used to find start of POST body
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

  ;; String utilities specific to HTTP/URI handling

  ;; Removes whitespace from start and end of string
  ;;
  ;; str - String to trim
  ;;
  ;; Returns: Trimmed string
  ;;
  ;; Notes:
  ;; - Returns empty string for null/empty input
  ;; - Handles both spaces and tabs
  (define (string-trim str)
    (let* ((start (string-skip str char-whitespace?))
           (end (string-skip-right str char-whitespace?)))
      (if (or (not start) (not end))
          ""
          (substring str start (+ end 1)))))

  ;; Checks if string starts with prefix
  ;;
  ;; prefix - String to look for
  ;; str    - String to check in
  ;;
  ;; Returns: #t if str starts with prefix, #f otherwise
  ;;
  ;; Notes:
  ;; - Case sensitive
  ;; - False if prefix longer than string
  (define (string-prefix? prefix str)
    (and (>= (string-length str) (string-length prefix))
         (string=? (substring str 0 (string-length prefix)) prefix)))

  ;; Finds first character not matching predicate
  ;;
  ;; str  - String to search
  ;; pred - Character predicate function
  ;;
  ;; Returns: Index of first non-matching char or #f
  ;;
  ;; Example:
  ;; (string-skip "  abc" char-whitespace?) -> 2
  (define (string-skip str pred)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((not (pred (string-ref str i))) i)
        (else (loop (+ i 1))))))

  ;; Like string-skip but from right to left
  ;;
  ;; str  - String to search
  ;; pred - Character predicate function
  ;;
  ;; Returns: Index of last non-matching char or #f
  ;;
  ;; Example:
  ;; (string-skip-right "abc  " char-whitespace?) -> 2
  (define (string-skip-right str pred)
    (let loop ((i (- (string-length str) 1)))
      (cond
        ((< i 0) #f)
        ((not (pred (string-ref str i))) i)
        (else (loop (- i 1))))))

  ;; Decodes URL-encoded string
  ;;
  ;; str - String to decode
  ;;
  ;; Returns: Decoded string
  ;;
  ;; Examples:
  ;; "hello+world" -> "hello world"
  ;; "hello%20world" -> "hello world"
  ;;
  ;; Notes:
  ;; - Handles both + and %XX encodings
  ;; - %XX must be valid hex digits
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
