(import (chezscheme))

(library-directories
  (cons (string-append (current-directory) "/src")
        (library-directories)))

(import (app main))
(run-app)
