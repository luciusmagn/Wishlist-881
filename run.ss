;;! Application entry point
;;!
;;! This is the main program file that:
;;! - Imports required libraries
;;! - Starts the HTTP server
;;! - Handles program initialization
;;!
;;! To run the server:
;;! chez --libdirs src --program run.ss
;;!
;;! Note: The --libdirs flag is required to find our libraries, since library-directories does not work in compiled

(import (chezscheme))

(library-directories
  (cons (string-append (current-directory) "/src")
        (library-directories)))

(import (app main))
(run-app)
