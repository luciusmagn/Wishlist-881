(library (app main)
  (export run-app)
  (import (chezscheme)
          (lib server)
          (lib handler)
          (lib storage)
          (lib auth)
          (lib http)
          (lib utils))

  (define (base-html content)
    (string-append
     "<!DOCTYPE html>
      <html>
        <head>
          <title>Wishlist</title>
          <script src=\"https://unpkg.com/htmx.org@2.0.0\"></script>
          <meta charset=\"UTF-8\">
        </head>
        <body>" content "</body>
      </html>"))

  (define (get-user headers)
    (let* ((cookies (get-cookies headers))
           (session (assoc "session" cookies)))
      (and session
           (valid-session? (cdr session))
           (get-session (cdr session)))))

  (define (main-page username lists)
    (base-html
     (string-append
      "<main>
         <h1>Wishlists</h1>
         <div class=\"user-info\">
           Logged in as " username "
           <form style=\"display:inline\" method=\"post\" action=\"/logout\">
             <button>Logout</button>
           </form>
         </div>
         <div id=\"lists\">"
      (lists-html lists username)
      "<button hx-post=\"/lists\"
               hx-target=\"#lists\"
               hx-swap=\"beforeend\">New List</button>
       </div>
       </main>")))

  (define (lists-html lists viewer)
    (apply string-append
           (map (lambda (list)
                  (list-html list viewer))
                lists)))

  (define (list-html list viewer)
    (let ((id (cdr (assq 'id list)))
          (title (cdr (assq 'title list)))
          (owner (cdr (assq 'owner list))))
      (string-append
       "<div class=\"list\">
      <h3>" title "</h3>
      <p>By: " owner "</p>"
      (if (string=? owner viewer)
          (string-append
           "<button hx-post=\"/items/new/"
           id
           "\" hx-target=\"#items-" id "\"
              hx-swap=\"afterbegin\">Add Item</button>")
          "")
      "<div class=\"items\" id=\"items-" id "\">"
      (apply string-append
             (map (lambda (item)
                    (item-html item (string=? owner viewer)))
                  (get-items id)))
      "</div>"
      "</div>")))

  (define (new-list)
    "<div class=\"list-form\">
       <input type=\"text\"
              name=\"title\"
              placeholder=\"List name\">
       <button hx-post=\"/lists/save\"
               hx-include=\"[name='title']\"
               hx-target=\"closest .list-form\">
         Save
       </button>
     </div>")

  (define (login-page)
    (base-html
     "<main>
        <h1>Login</h1>
        <form method=\"post\" action=\"/login\">
          <input type=\"text\" name=\"username\" placeholder=\"Username\">
          <input type=\"password\" name=\"password\" placeholder=\"Password\">
          <button type=\"submit\">Login</button>
        </form>
        <p>
          <a href=\"/register\">Register</a>
        </p>
      </main>"))

  (define (register-page)
    (base-html
     "<main>
        <h1>Register</h1>
        <form method=\"post\" action=\"/register\">
          <input type=\"text\" name=\"username\" placeholder=\"Username\">
          <input type=\"password\" name=\"password\" placeholder=\"Password\">
          <button type=\"submit\">Register</button>
        </form>
      </main>"))

  (define (new-item-form list-id)
    (string-append
     "<div class=\"item-form\">
       <input type=\"text\"
              name=\"name\"
              placeholder=\"Item name\">
       <button hx-post=\"/items/save/" list-id "\"
               hx-include=\"[name='name']\"
               hx-target=\"closest .items\"
               hx-swap=\"beforeend\">
         Save
       </button>
     </div>"))

  (define (save-item-response item list-id viewer-is-owner)
    (string-append
     "<div hx-swap-oob=\"afterbegin:#items-" list-id "\">"
     (item-html item viewer-is-owner)
     "</div>"
     "<div hx-swap-oob=\"delete:.item-form\"></div>"))

  (define (item-html item viewer-is-owner)
    (let ((name (cdr (assq 'name item)))
          (claimed (cdr (assq 'claimed-by item)))
          (received (cdr (assq 'received item)))
          (id (cdr (assq 'id item))))
      (if viewer-is-owner
          (string-append
           "<div class=\"item\">" name
           (if received
               " (Received)"
               (string-append
                " <button hx-post=\"/items/" id "/received\"
                       hx-target=\"closest .item\"
                       hx-swap=\"outerHTML\">Got it</button>"))
           " <button hx-delete=\"/items/" id "\"
                   hx-target=\"closest .item\"
                   hx-swap=\"outerHTML\">Delete</button></div>")
          (string-append
           "<div class=\"item\">" name
           (if claimed
               " (Already claimed)"
               (string-append
                " <button hx-post=\"/items/claim/"
                id
                "\" hx-target=\"closest .item\"
              hx-swap=\"outerHTML\">I'll give this!</button>"))
           "</div>"))))

  (define (claim-item-response item)
    (string-append
     "<div class=\"item\">"
     (cdr (assq 'name item))
     " (Already claimed)</div>"))

  (define (run-app)
    (let ((home (make-empty-handler "/"))
          (login (make-empty-handler "/login"))
          (register (make-empty-handler "/register"))
          (logout (make-empty-handler "/logout"))
          (lists (make-empty-handler "/lists"))
          (save-list (make-empty-handler "/lists/save"))
          (new-item (make-empty-handler "/items/new/:list-id"))
          (save-item (make-empty-handler "/items/save/:list-id"))
          (claim-item (make-empty-handler "/items/claim/:item-id"))
          (mark-received (make-empty-handler "/items/:item-id/received"))
          (delete-item (make-empty-handler "/items/:item-id")))


      (register-plain-handler! home 'get
                               (lambda (client headers data params)  ; Added params
                                 (let ((session (get-user headers)))
                                   (if session
                                       (let ((username (cdr (assq 'username session))))
                                         (send-response client 200
                                                        (main-page username (get-lists))))
                                       (send-response client 200
                                                      (login-page))))))

      (register-plain-handler! register 'get
                               (lambda (client headers data params)  ; Added params
                                 (send-response client 200 (register-page))))

      (register-plain-handler! login 'post
                               (lambda (client headers data params)  ; Added params
                                 (let* ((params (get-post-data headers data))
                                        (username (cdr (assoc "username" params)))
                                        (password (cdr (assoc "password" params))))
                                   (if (validate-user username password)
                                       (let ((session-id (create-session! username)))
                                         (send-response client 200
                                                        (main-page username (get-lists))
                                                        (make-cookie "session" session-id)))
                                       (send-response client 401 (login-page))))))

      (register-plain-handler! register 'post
                               (lambda (client headers data params)  ; Added params
                                 (let* ((params (get-post-data headers data))
                                        (username (cdr (assoc "username" params)))
                                        (password (cdr (assoc "password" params))))
                                   (create-user! username password)
                                   (send-response client 200 (login-page)))))

      (register-plain-handler! logout 'post
                               (lambda (client headers data params)
                                 (let* ((cookies (get-cookies headers))
                                        (session (assoc "session" cookies)))
                                   (when session
                                     (invalidate-session! (cdr session))))
                                 (send-response client 302
                                                ""
                                                "Location: /"
                                                (make-cookie "session" "deleted; expires=Thu, 01 Jan 1970 00:00:00 GMT"))))

      (register-hx-handler! lists 'post
                            (lambda (client headers data params)
                              (let ((session (get-user headers)))
                                (if session
                                    (send-response client 200 (new-list))
                                    (send-response client 401 "Login required")))))

      (register-hx-handler! save-list 'post
        (lambda (client headers data params)
          (let* ((session (get-user headers))
                 (params (get-post-data headers data))
                 (title (cdr (assoc "title" params))))
            (if session
                (let* ((username (cdr (assq 'username session)))
                       (id (make-list! title username)))
                  (send-response client 200
                    (list-html `((id . ,id)
                                (title . ,title)
                                (owner . ,username))
                              username)))
                (send-response client 401 "Login required")))))

      (register-hx-handler! new-item 'post
                            (lambda (client headers data params)
                              (display "New item params: ") (display params) (newline)
                              (let* ((session (get-user headers))
                                     (list-id (cdr (assoc 'list-id params))))  ; Keep the : for now
                                (if session
                                    (send-response client 200
                                                   (new-item-form list-id))
                                    (send-response client 401 "Login required")))))

      (register-hx-handler! save-item 'post
                            (lambda (client headers data params)
                              (let* ((session (get-user headers))
                                     (list-id (cdr (assoc 'list-id params)))
                                     (post-params (get-post-data headers data))
                                     (name (cdr (assoc "name" post-params))))
                                (if (and session name)
                                    (begin
                                      (let ((id (add-item! list-id name)))
                                        (send-response client 200
                                                       (save-item-response
                                                        `((id . ,id)
                                                          (name . ,name)
                                                          (claimed-by . #f)
                                                          (received . #f))  ; Add received field
                                                        list-id
                                                        #t))))
                                    (send-response client 401 "Login required")))))

      (register-hx-handler! claim-item 'post
                            (lambda (client headers data params)
                              (let* ((session (get-user headers))
                                     (item-id (cdr (assoc 'item-id params))))
                                (if session
                                    (let* ((username (cdr (assq 'username session)))
                                           (item (claim-item! item-id username)))
                                      (send-response client 200
                                                     (claim-item-response item)))
                                    (send-response client 401 "Login required")))))

      (register-hx-handler! mark-received 'post
                            (lambda (client headers data params)
                              (let* ((session (get-user headers))
                                     (item-id (cdr (assoc 'item-id params))))
                                (if session
                                    (let ((item (mark-received! item-id)))
                                      (send-response client 200
                                                     (item-html item #t)))
                                    (send-response client 401 "Login required")))))

      (register-hx-handler! delete-item 'delete
                            (lambda (client headers data params)
                              (let* ((session (get-user headers))
                                     (item-id (cdr (assoc 'item-id params))))
                                (if session
                                    (begin
                                      (delete-item! item-id)
                                      (send-response client 200 ""))
                                    (send-response client 401 "Login required")))))

      (run-server 8080
                  (list home login register logout
                        lists save-list
                        new-item save-item claim-item
                        mark-received delete-item)))))
