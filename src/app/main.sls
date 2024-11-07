(library (app main)
  (export run-app)
  (import (chezscheme)
          (lib server)
          (lib handler))

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

  (define (main-page)
    (base-html
     "<main>
        <h1>My Wishlists</h1>
        <div id=\"lists\">
          <button hx-post=\"/lists\"
                  hx-target=\"#lists\"
                  hx-swap=\"beforeend\">New List</button>
          <div id=\"wishlist-container\"></div>
        </div>
      </main>"))

  (define (new-list)
    "<div class=\"list\">
       <input type=\"text\"
              name=\"title\"
              placeholder=\"List name\">
       <button hx-post=\"/lists/save\"
               hx-include=\"[name='title']\"
               hx-target=\"closest .list\">
         Save
       </button>
     </div>")

  (define (run-app)
    (let ((home (make-empty-handler "/"))
          (lists (make-empty-handler "/lists"))
          (save-list (make-empty-handler "/lists/save")))

      ;; Main page
      (register-plain-handler! home 'get
        (lambda (client)
          (send-response client 200 (main-page))))

      ;; New list form
      (register-hx-handler! lists 'post
        (lambda (client)
          (send-response client 200 (new-list))))

      ;; Save list
      (register-hx-handler! save-list 'post
        (lambda (client)
          (send-response client 200
            "<div class=\"saved-list\">
               <h3>New List Created!</h3>
               <button hx-post=\"/items/new\"
                       hx-target=\"closest .saved-list\">
                 Add Item
               </button>
             </div>")))

      (run-server 8080 (list home lists save-list)))))
