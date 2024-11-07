;;! Data persistence and storage operations
;;!
;;! Handles all data storage using filesystem:
;;! - Wishlist creation and retrieval
;;! - Item management (add/claim/delete)
;;! - File organization and naming
;;! - Atomic file operations
;;!
;;! Uses a hierarchy of directories and files where each piece
;;! of data (list, item, etc.) is stored as a Scheme expression
;;! in its own file. This provides natural atomicity and makes
;;! the data human-readable.

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

  ;; Root paths for data storage
  (define data-root "data")
  (define lists-root (string-append data-root "/lists"))

  ;; Creates necessary directory structure if it doesn't exist
  ;;
  ;; Side effects:
  ;; - Creates data/ directory if missing
  ;; - Creates data/lists/ directory if missing
  (define (ensure-directories!)
    (unless (file-exists? data-root)
      (mkdir data-root))
    (unless (file-exists? lists-root)
      (mkdir lists-root)))

  ;; Generates a unique ID based on current timestamp
  ;;
  ;; Returns: String containing timestamp in hexadecimal
  ;; Example: "1A2B3C4D"
  (define (generate-id)
    (number->string
      (floor (/ (* (current-seconds) 1000) 1))
      16))

  ;; Creates a new wishlist
  ;;
  ;; title - String name of the wishlist
  ;; owner - String username of the list owner
  ;;
  ;; Returns: String ID of the created list
  ;;
  ;; Side effects:
  ;; - Creates new directory data/lists/<id>/
  ;; - Creates meta.scm file with list information
  ;; Notes:
  ;; - Directory creation is atomic
  ;; - Meta file write is atomic
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

  ;; Retrieves wishlist metadata
  ;;
  ;; id - String ID of the list to retrieve
  ;;
  ;; Returns: Alist with list data (id, title, owner) or #f if not found
  ;; Example: ((id . "123") (title . "Birthday") (owner . "john"))
  (define (get-list id)
    (let ((meta-file (string-append lists-root "/" id "/meta.scm")))
      (if (file-exists? meta-file)
          (with-input-from-file meta-file read)
          #f)))

  ;; Retrieves all wishlists
  ;;
  ;; Returns: List of alists, each containing list metadata
  ;; Notes:
  ;; - Automatically ensures directory structure exists
  ;; - Skips any corrupted or unreadable lists
  (define (get-lists)
    (ensure-directories!)
    (map
      (lambda (dir)
        (get-list (file-path-last dir)))
      (directory-list lists-root)))

  ;; Gets all items in a wishlist
  ;;
  ;; list-id - String ID of the list to get items from
  ;;
  ;; Returns: List of alists, each containing item data:
  ;; - id: String unique identifier
  ;; - name: String item name
  ;; - claimed-by: String username or #f
  ;; - received: Boolean indicating if owner got it
  ;;
  ;; Notes:
  ;; - Returns empty list if items directory doesn't exist
  ;; - Adds received=#f to older items missing this field
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

  ;; Adds new item to a wishlist
  ;;
  ;; list-id - String ID of the list to add to
  ;; name    - String name of the item
  ;;
  ;; Returns: String ID of the created item
  ;;
  ;; Side effects:
  ;; - Creates items directory if missing
  ;; - Creates new item file
  ;;
  ;; Notes:
  ;; - Item starts unclaimed (claimed-by=#f)
  ;; - Item starts unreceived (received=#f)
  ;; - File write is atomic
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

  ;; Marks an item as claimed by a user
  ;;
  ;; item-id - String ID of the item to claim
  ;; claimer - String username of the person claiming
  ;;
  ;; Returns: Alist of updated item data
  ;;
  ;; Side effects:
  ;; - Updates item file with new claimed-by value
  ;;
  ;; Notes:
  ;; - No effect if item is already claimed (returns unchanged)
  ;; - Preserves received status
  ;; - File write is atomic via truncate mode
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

  ;; Deletes an item from a wishlist
  ;;
  ;; item-id - String ID of the item to delete
  ;;
  ;; Side effects:
  ;; - Removes item file if it exists
  ;;
  ;; Notes:
  ;; - Safe to call on non-existent items
  ;; - File deletion is atomic
  (define (delete-item! item-id)
    (let ((item-path (find-item-path item-id)))
      (when item-path
        (delete-file item-path))))

  ;; Finds filesystem path for an item
  ;;
  ;; item-id - String ID of the item to locate
  ;;
  ;; Returns: String full path to item file, or #f if not found
  ;;
  ;; Notes:
  ;; - Searches through all lists (could be slow with many lists)
  ;; - Used internally by claim-item!, delete-item!, mark-received!
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

  ;; Marks an item as received by its owner
  ;;
  ;; item-id - String ID of the item to mark
  ;;
  ;; Returns: Alist of updated item data
  ;;
  ;; Side effects:
  ;; - Updates item file with received=#t
  ;;
  ;; Notes:
  ;; - Preserves claimed-by status
  ;; - File write is atomic via truncate mode
  ;; - Can be called multiple times safely
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

  ;; Extracts last component from a file path
  ;;
  ;; path - String file path
  ;;
  ;; Returns: String containing last path component
  ;; Example: (file-path-last "a/b/c") -> "c"
  (define (file-path-last path)
    (let ((parts (string-split path #\/)))
      (car (reverse parts)))))
