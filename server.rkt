#lang web-server/insta

; A blog is a (listof post)
; and a post is a (post title body)
(struct post (title body))

; BLOG: blog
; The static blog.
; Can be defined alternatively i.e.
; (define BLOG (list (post "a1" "b1") (post "a2" "b2")))
(define BLOG
  `(
   ,(post "Second Post" "This is another post")
   ,(post "First Post" "This is my first post")))

; render-greeting: string -> response
; Consumes a name, and produces a dynamic response.
(define (render-greeting a-name)
  (response/xexpr
   `(html (head (title "Welcome"))
          (body (p ,(string-append "Hello " a-name))))))

; render-as-item: xexpr -> xexpr
; Consumes an xexpr, and produces a rendering
; as a list item.
(define (render-as-item a-fragment)
  `(li ,a-fragment))

; render-as-itemized-list: (listof xexpr) -> xexpr
; Consumes a list of items, and produces a rendering
; as an unordered list.
(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

; render-greeting: string -> response
; Consumes a name, and produces a dynamic response.
(define (render-post a-post)
  `(div ((class "post")) ,(post-title a-post) (p ,(post-body a-post))))

; render-posts : ((listof post?) . -> . xexpr/c)
(define (render-posts a-blog)
  `(div ((class "posts")) ,@(map render-post a-blog)))

; render-blog-page: blog request -> response
; Consumes a blog and a request, and produces an HTML page
; of the content of the blog.
(define (render-blog-page a-blog request)
  (response/xexpr
   `(html (head (title "My Blog"))
          (body
           (h1 "My Blog")
           ,(render-posts a-blog)
           (form
            (input ((name "title")))
            (input ((name "body")))
            (input ((type "submit"))))))))

; can-parse-post?: bindings -> boolean
; Produces true if bindings contains values for 'title and 'body.
(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
       (exists-binding? 'body bindings)))

; parse-post: bindings -> post
; Consumes a bindings, and produces a post out of the bindings.
(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)))

; start: request -> response
; Consumes a request, and produces a page that displays all of the
; web content.
(define (start request)
  (define a-blog
    (cond [(can-parse-post? (request-bindings request))
           (cons (parse-post (request-bindings request))
                 BLOG)]
          [else
           BLOG]))
  (render-blog-page a-blog request))