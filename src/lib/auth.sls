;;! Authentication and session management
;;!
;;! Handles user authentication and session lifecycle:
;;! - User account creation and validation
;;! - Session creation and verification
;;! - Session storage and cleanup
;;!
;;! Uses simple file-based storage where each user and session
;;! is stored as a Scheme expression in its own file. While not
;;! secure for production use, it provides a simple mechanism
;;! for learning and development.
;;!
;;! Note: This is NOT a secure implementation - passwords are stored
;;! in plaintext and sessions use predictable IDs.

(library (lib auth)
  (export create-session!
          get-session
          valid-session?
          invalidate-session!
          create-user!
          validate-user)
  (import (chezscheme)
          (lib utils))

  (define sessions-root "data/sessions")
  (define users-root "data/users")

  ;; Creates required directory structure
  ;;
  ;; Side effects:
  ;; - Creates data/ if missing
  ;; - Creates data/sessions/ if missing
  ;; - Creates data/users/ if missing
  ;;
  ;; Notes:
  ;; - Called internally before file operations
  ;; - Safe to call multiple times
  (define (ensure-dirs!)
    (unless (file-exists? "data")
      (mkdir "data"))
    (unless (file-exists? sessions-root)
      (mkdir sessions-root))
    (unless (file-exists? users-root)
      (mkdir users-root)))

  ;; Creates new user account
  ;;
  ;; username - String username for new account
  ;; password - String password (stored in plaintext)
  ;;
  ;; Side effects:
  ;; - Creates user file in data/users/
  ;;
  ;; Notes:
  ;; - Overwrites existing user file if username exists
  ;; - File write is atomic via native filesystem
  (define (create-user! username password)
    (ensure-dirs!)
    (let ((user-file (string-append users-root "/" username ".scm")))
      (with-output-to-file user-file
        (lambda ()
          (write `((username . ,username)
                  (password . ,password)))))))

  ;; Validates user login credentials
  ;;
  ;; username - String username to check
  ;; password - String password to verify
  ;;
  ;; Returns: #t if credentials valid, #f otherwise
  ;;
  ;; Notes:
  ;; - Uses direct string comparison (no hashing)
  ;; - Returns #f if user doesn't exist
  (define (validate-user username password)
    (let ((user-file (string-append users-root "/" username ".scm")))
      (and (file-exists? user-file)
           (let ((user (with-input-from-file user-file read)))
             (string=? (cdr (assq 'password user)) password)))))

  ;; Creates new session for authenticated user
  ;;
  ;; username - String username to create session for
  ;;
  ;; Returns: String session ID
  ;;
  ;; Side effects:
  ;; - Creates session file in data/sessions/
  ;;
  ;; Notes:
  ;; - Session ID is based on timestamp (predictable)
  ;; - Session stored as Scheme expression with creation time
  (define (create-session! username)
    (ensure-dirs!)
    (let* ((session-id (number->string
                        (floor (/ (* (current-seconds) 1000) 1))
                        16))
           (session-file (string-append sessions-root "/" session-id ".scm")))
      (with-output-to-file session-file
        (lambda ()
          (write `((id . ,session-id)
                  (username . ,username)
                  (created . ,(current-seconds))))))
      session-id))

  ;; Invalidates (deletes) existing session
  ;;
  ;; session-id - String ID of session to invalidate
  ;;
  ;; Side effects:
  ;; - Deletes session file if it exists
  ;;
  ;; Notes:
  ;; - Safe to call on non-existent sessions
  ;; - File deletion is atomic
  (define (invalidate-session! session-id)
    (let ((session-file (string-append sessions-root "/" session-id ".scm")))
      (when (file-exists? session-file)
        (delete-file session-file))))

  ;; Retrieves session data
  ;;
  ;; session-id - String ID of session to retrieve
  ;;
  ;; Returns: Alist of session data or #f if not found
  ;; Example: ((id . "abc123") (username . "john") (created . 1704587428))
  (define (get-session session-id)
    (let ((session-file (string-append sessions-root "/" session-id ".scm")))
      (and (file-exists? session-file)
           (with-input-from-file session-file read))))

  ;; Checks if session is valid (exists and not expired)
  ;;
  ;; session-id - String ID of session to verify
  ;;
  ;; Returns: #t if session exists and not expired, #f otherwise
  ;;
  ;; Notes:
  ;; - Sessions expire after 24 hours
  ;; - Expired sessions are not automatically deleted
  (define (valid-session? session-id)
    (let ((session (get-session session-id)))
      (and session
           (< (- (current-seconds)
                 (cdr (assq 'created session)))
              (* 24 60 60))))))  ; 24 hour expiry
