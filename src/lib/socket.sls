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

  ;; Load libc for socket operations
  ;; Note: This requires libc.so.6 to be present on the system
  (define load-socket-lib
    (load-shared-object "libc.so.6"))

  ;; Constants from sys/socket.h
  (define AF_INET 2)        ; IPv4 Internet protocols
  (define SOCK_STREAM 1)    ; Sequential, reliable, two-way connection streams
  (define SOL_SOCKET 1)     ; Socket level for options
  (define SO_REUSEADDR 2)   ; Allow reuse of local addresses


  ;; Foreign function bindings to libc socket API

  ;; Creates new socket
  ;; Parameters correspond to C function:
  ;; int socket(int domain, int type, int protocol)
  (define socket
    (foreign-procedure "socket" (int int int) int))

  ;; Binds socket to address
  ;; Parameters correspond to C function:
  ;; int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
  (define bind
    (foreign-procedure "bind" (int void* int) int))

  ;; Marks socket as passive
  ;; Parameters correspond to C function:
  ;; int listen(int sockfd, int backlog)
  (define listen
    (foreign-procedure "listen" (int int) int))

  ;; Accepts incoming connection
  ;; Parameters correspond to C function:
  ;; int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen)
  (define accept
    (foreign-procedure "accept" (int void* void*) int))

  ;; Sets socket options
  ;; Parameters correspond to C function:
  ;; int setsockopt(int sockfd, int level, int optname,
  ;;                const void *optval, socklen_t optlen)
  (define setsockopt
    (foreign-procedure "setsockopt" (int int int void* int) int))

  ;; Creates a server socket bound to given port
  ;;
  ;; port - Integer port number to bind to
  ;;
  ;; Returns: Integer socket file descriptor
  ;;
  ;; Side effects:
  ;; - Allocates system socket
  ;; - Binds to network interface
  ;;
  ;; Notes:
  ;; - Sets SO_REUSEADDR to prevent "address already in use"
  ;; - Uses AF_INET (IPv4) and SOCK_STREAM (TCP)
  ;; - Backlog of 5 connections
  ;; - Throws error if bind or listen fails
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

  ;; Accepts new connection on server socket
  ;;
  ;; server-sock - Integer socket file descriptor from make-server-socket
  ;;
  ;; Returns: Integer file descriptor for new connection
  ;;
  ;; Notes:
  ;; - Blocks until connection arrives
  ;; - Allocates memory for address structures
  (define (accept-connection server-sock)
    (let ([addr (foreign-alloc 16)]
          [addr-len (foreign-alloc 4)])
      (foreign-set! 'int addr-len 0 16)
      (accept server-sock addr addr-len)))

  ;; Sends data over socket
  ;;
  ;; sock - Integer socket file descriptor
  ;; data - String data to send
  ;;
  ;; Returns: Integer number of bytes sent
  ;;
  ;; Notes:
  ;; - Uses send() with no flags
  ;; - Does not guarantee all data is sent
  (define (socket-send sock data)
    (let ([send (foreign-procedure "send" (int string int int) int)])
      (send sock data (string-length data) 0)))

  ;; Receives data from socket
  ;;
  ;; sock - Integer socket file descriptor
  ;;
  ;; Returns: String containing received data
  ;;
  ;; Notes:
  ;; - Uses 4KB buffer
  ;; - Returns empty string on error/connection closed
  ;; - Converts received bytes to UTF-8 string
  (define (socket-recv sock)
    (let* ([recv (foreign-procedure "recv" (int u8* int int) int)]
           [buf (make-bytevector 4096)]
           [n (recv sock buf 4096 0)])
      (if (> n 0)
          (utf8->string (bytevector-slice buf 0 n))
          "")))

  ;; Extracts slice of bytevector
  ;;
  ;; bv    - Source bytevector
  ;; start - Starting index
  ;; end   - Ending index (exclusive)
  ;;
  ;; Returns: New bytevector containing slice
  (define (bytevector-slice bv start end)
  (let ([new (make-bytevector (- end start))])
    (bytevector-copy! bv start new 0 (- end start))
    new))

  ;; Closes a socket
  ;;
  ;; sock - Integer socket file descriptor
  ;;
  ;; Returns: Result of close operation (0 on success)
  (define close-socket
    (foreign-procedure "close" (int) int))

  ;; Converts host byte order to network byte order (16-bit)
  ;;
  ;; x - Integer to convert
  ;;
  ;; Returns: Integer in network byte order
  ;;
  ;; Notes:
  ;; - Swaps bytes on little-endian systems
  ;; - No effect on big-endian systems
  ;;
  ;; I STOLE THIS
  (define (network-byte-order x)
    (+ (fxsll (fxand x #xff) 8)
       (fxsrl x 8))))
