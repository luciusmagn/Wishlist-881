;;! Authentication and session management
;;!
;;! Handles all user-related security aspects:
;;! - User account creation and storage
;;! - Password verification (plaintext - NOT FOR PRODUCTION!)
;;! - Session creation and validation
;;! - Session cleanup and logout
;;!
;;! Uses simple file-based storage where:
;;! - Each user is a separate file in data/users/
;;! - Each session is a separate file in data/sessions/
;;! - Files contain Scheme expressions for easy reading/writing

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

  (define (ensure-dirs!)
    (unless (file-exists? "data")
      (mkdir "data"))
    (unless (file-exists? sessions-root)
      (mkdir sessions-root))
    (unless (file-exists? users-root)
      (mkdir users-root)))

  (define (create-user! username password)
    (ensure-dirs!)
    (let ((user-file (string-append users-root "/" username ".scm")))
      (with-output-to-file user-file
        (lambda ()
          (write `((username . ,username)
                  (password . ,password)))))))

  (define (validate-user username password)
    (let ((user-file (string-append users-root "/" username ".scm")))
      (and (file-exists? user-file)
           (let ((user (with-input-from-file user-file read)))
             (string=? (cdr (assq 'password user)) password)))))

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

  (define (invalidate-session! session-id)
    (let ((session-file (string-append sessions-root "/" session-id ".scm")))
      (when (file-exists? session-file)
        (delete-file session-file))))

  (define (get-session session-id)
    (let ((session-file (string-append sessions-root "/" session-id ".scm")))
      (and (file-exists? session-file)
           (with-input-from-file session-file read))))

  (define (valid-session? session-id)
    (let ((session (get-session session-id)))
      (and session
           (< (- (current-seconds)
                 (cdr (assq 'created session)))
              (* 24 60 60))))))  ; 24 hour expiry
