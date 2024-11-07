;;! Low-level socket operations
;;!
;;! Direct bindings to libc socket functions:
;;! - Socket creation and configuration
;;! - Connection acceptance
;;! - Data sending/receiving
;;! - Network byte order handling
;;!
;;! This is our lowest level module, providing the network I/O
;;! that everything else builds upon. It uses Chez's foreign
;;! function interface to directly call libc functions.

(library (lib socket)
  (export make-server-socket
          accept-connection
          socket-send
          socket-recv
          close-socket)
  (import (chezscheme))

  (define load-socket-lib
    (load-shared-object "libc.so.6"))

  (define AF_INET 2)
  (define SOCK_STREAM 1)
  (define SOL_SOCKET 1)
  (define SO_REUSEADDR 2)

  (define socket
    (foreign-procedure "socket" (int int int) int))

  (define bind
    (foreign-procedure "bind" (int void* int) int))

  (define listen
    (foreign-procedure "listen" (int int) int))

  (define accept
    (foreign-procedure "accept" (int void* void*) int))

  (define setsockopt
    (foreign-procedure "setsockopt" (int int int void* int) int))

  (define (make-server-socket port)
    (let ([sock (socket AF_INET SOCK_STREAM 0)]
          [addr (foreign-alloc 16)])

      ;; Set SO_REUSEADDR
      (let ([opt (foreign-alloc 4)])
        (foreign-set! 'int opt 0 1)
        (setsockopt sock SOL_SOCKET SO_REUSEADDR opt 4))

      ;; Setup address structure
      (foreign-set! 'unsigned-16 addr 0 AF_INET)
      (foreign-set! 'unsigned-16 addr 2 (network-byte-order port))
      (foreign-set! 'unsigned-32 addr 4 0)

      (when (< (bind sock addr 16) 0)
        (error 'make-server-socket "bind failed"))

      (when (< (listen sock 5) 0)
        (error 'make-server-socket "listen failed"))

      sock))

  (define (accept-connection server-sock)
    (let ([addr (foreign-alloc 16)]
          [addr-len (foreign-alloc 4)])
      (foreign-set! 'int addr-len 0 16)
      (accept server-sock addr addr-len)))

  (define (socket-send sock data)
    (let ([send (foreign-procedure "send" (int string int int) int)])
      (send sock data (string-length data) 0)))

  (define (socket-recv sock)
    (let* ([recv (foreign-procedure "recv" (int u8* int int) int)]
           [buf (make-bytevector 4096)]
           [n (recv sock buf 4096 0)])
      (if (> n 0)
          (utf8->string (bytevector-slice buf 0 n))
          "")))

  (define (bytevector-slice bv start end)
  (let ([new (make-bytevector (- end start))])
    (bytevector-copy! bv start new 0 (- end start))
    new))

  (define close-socket
    (foreign-procedure "close" (int) int))

  (define (network-byte-order x)
    (+ (fxsll (fxand x #xff) 8)
       (fxsrl x 8))))
