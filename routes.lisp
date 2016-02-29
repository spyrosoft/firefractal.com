(defvar *this-file* #.(or *compile-file-pathname* *load-truename*))

(load "utilities/routing.lisp")

(load "acceptors/buy-poster.lisp")
(load "acceptors/feedback-form.lisp")

;; Route / to index.html and serve it as text/html
(route-file "/" "/" "index" "html" "text/html")

(route-file "/how-to.html" "/" "how-to" "html" "text/html")
(route-file "/about.html" "/" "about" "html" "text/html")
(route-file "/gallery.html" "/" "gallery" "html" "text/html")
(route-file "/favicon.ico" "/images" "favicon" "ico" "vnd.microsoft.icon")

;; Route everything in /css/ to the /css directory and serve it as text/html
(route-directory "/css/" "/css" "text/css")

;; Route everything in /js/ to the /js directory and serve it as text/javascript
(route-directory "/js/" "/js" "text/javascript")

;; Route everything in /images/ to the /images directory and serve it as whatever mime type is appropriate
(route-directory "/images/" "/images")

;; Route /robots.txt to the actual file and serve it as text/plain
(route-file "/robots.txt" "/" "robots" "txt" "text/plain")