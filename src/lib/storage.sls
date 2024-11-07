(library (lib storage)
  (export make-list!
          add-item!
          get-list
          get-lists
          get-items
          claim-item!
          delete-item!
          mark-received!)
  (import (chezscheme)
          (lib utils))

  ;; Paths
  (define data-root "data")
  (define lists-root (string-append data-root "/lists"))

  ;; Utils
  (define (ensure-directories!)
    (unless (file-exists? data-root)
      (mkdir data-root))
    (unless (file-exists? lists-root)
      (mkdir lists-root)))

  (define (generate-id)
    (number->string
      (floor (/ (* (current-seconds) 1000) 1))
      16))

  ;; List operations
  (define (make-list! title owner)
    (ensure-directories!)
    (let* ((id (generate-id))
           (list-dir (string-append lists-root "/" id)))
      (mkdir list-dir)
      (with-output-to-file
        (string-append list-dir "/meta.scm")
        (lambda ()
          (write `((id . ,id)
                  (title . ,title)
                  (owner . ,owner)))))
      id))

  (define (get-list id)
    (let ((meta-file (string-append lists-root "/" id "/meta.scm")))
      (if (file-exists? meta-file)
          (with-input-from-file meta-file read)
          #f)))

  (define (get-lists)
    (ensure-directories!)
    (map
      (lambda (dir)
        (get-list (file-path-last dir)))
      (directory-list lists-root)))

  (define (get-items list-id)
    (let ((items-dir (string-append lists-root "/" list-id "/items")))
      (if (file-exists? items-dir)
          (map (lambda (file)
                 (let ((item (with-input-from-file
                                 (string-append items-dir "/" file)
                               read)))
                   (if (assq 'received item)
                       item
                       (cons '(received . #f) item))))  ; Add if missing
               (directory-list items-dir))
          '())))

  ;; Item operations
  (define (add-item! list-id name)
    (let* ((id (generate-id))
           (list-dir (string-append lists-root "/" list-id))
           (items-dir (string-append list-dir "/items")))
      (unless (file-exists? items-dir)
        (mkdir items-dir))
      (with-output-to-file
        (string-append items-dir "/" id ".scm")
        (lambda ()
          (write `((id . ,id)
                  (name . ,name)
                  (claimed-by . #f)
                  (received . #f)))))
      id))

  (define (claim-item! item-id claimer)
    (let* ((item-path (find-item-path item-id))
           (item (with-input-from-file item-path read)))
      (if (cdr (assq 'claimed-by item))
          item  ; Already claimed, return unchanged
          (let ((updated-item `((id . ,(cdr (assq 'id item)))
                                (name . ,(cdr (assq 'name item)))
                                (claimed-by . ,claimer)
                                (received . ,(or (assq 'received item) #f)))))  ; Preserve received
            (call-with-output-file item-path
              (lambda (port) (write updated-item port))
              'truncate)
            updated-item))))

  (define (delete-item! item-id)
    (let ((item-path (find-item-path item-id)))
      (when item-path
        (delete-file item-path))))

  ;; Add helper to find item path
  (define (find-item-path item-id)
    (let loop ((list-dirs (directory-list lists-root)))
      (if (null? list-dirs)
          #f
          (let* ((list-dir (car list-dirs))
                 (items-dir (string-append lists-root "/" list-dir "/items"))
                 (item-path (string-append items-dir "/" item-id ".scm")))
            (if (file-exists? item-path)
                item-path
                (loop (cdr list-dirs)))))))

  (define (mark-received! item-id)
    (let* ((item-path (find-item-path item-id))
           (item (with-input-from-file item-path read)))
      (let ((updated-item `((id . ,(cdr (assq 'id item)))
                            (name . ,(cdr (assq 'name item)))
                            (claimed-by . ,(cdr (assq 'claimed-by item)))
                            (received . #t))))
        (call-with-output-file item-path
          (lambda (port) (write updated-item port))
          'truncate)
        updated-item)))

  ;; Path utils
  (define (file-path-last path)
    (let ((parts (string-split path #\/)))
      (car (reverse parts)))))
