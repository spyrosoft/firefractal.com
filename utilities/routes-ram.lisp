;(defvar *static-html-content* (make-hash-table :test #'equal))
;(defvar *static-css-content* (make-hash-table :test #'equal))
;(defvar *static-javascript-content* (make-hash-table :test #'equal))
;(defvar *static-image-content* (make-hash-table :test #'equal))
;
;(defvar *static-html-directory* "/home/path/firefractal/static")
;(defvar *static-css-directory* "/home/path/firefractal/static")
;(defvar *static-javascript-directory* "/home/path/firefractal/static")
;(defvar *static-image-directory* "/home/path/firefractal/static")
;
;(setf (gethash "index-html" *static-html-content*)
;			(load-static-content "index" "html" *static-html-directory*))

(defvar *this-file* (load-time-value
										 (or #.*compile-file-pathname* *load-pathname*)))



(defun load-html-content (html-content)
	(setf (content-type*) "text/html")
	html-content)

(defun load-css-content (html-content)
	(setf (content-type*) "text/css")
	html-content)

(defun load-js-content (html-content)
	(setf (content-type*) "text/javascript")
	html-content)

(defun load-svg-content (html-content)
	(setf (content-type*) "image/svg+xml")
	html-content)



(defvar *static-index-html*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static"
												:name "index"
												:type "html"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-index-html ()
	(load-html-content *static-index-html*))

(push (create-regex-dispatcher "^/$" 'mandelbrot-index-html) *dispatch-table*)



(defvar *static-styles-css*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/css"
												:name "styles"
												:type "css"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-styles-css ()
	(load-css-content *static-styles-css*))

(push (create-regex-dispatcher "^/css/styles.css$" 'mandelbrot-styles-css) *dispatch-table*)



(defvar *static-normalize-min-css*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/css"
												:name "normalize.min"
												:type "css"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-normalize-min-css ()
	(load-css-content *static-normalize-min-css*))

(push (create-regex-dispatcher "^/css/normalize.min.css$" 'mandelbrot-normalize-min-css) *dispatch-table*)



(defvar *static-foundation-min-css*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/css"
												:name "foundation.min"
												:type "css"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-foundation-min-css ()
	(load-css-content *static-foundation-min-css*))

(push (create-regex-dispatcher "^/css/foundation.min.css$" 'mandelbrot-foundation-min-css) *dispatch-table*)



(defvar *static-jquery-min-js*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/js"
												:name "jquery.min"
												:type "js"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-jquery-min-js ()
	(load-js-content *static-jquery-min-js*))

(push (create-regex-dispatcher "^/js/jquery.min.js$" 'mandelbrot-jquery-min-js) *dispatch-table*)



(defvar *static-foundation-min-js*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/js"
												:name "foundation.min"
												:type "js"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-foundation-min-js ()
	(load-js-content *static-foundation-min-js*))

(push (create-regex-dispatcher "^/js/foundation.min.js$" 'mandelbrot-foundation-min-js) *dispatch-table*)



(defvar *static-foundation-min-js*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/js"
												:name "foundation.min"
												:type "js"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-foundation-min-js ()
	(load-js-content *static-foundation-min-js*))

(push (create-regex-dispatcher "^/js/foundation.min.js$" 'mandelbrot-foundation-min-js) *dispatch-table*)



(defvar *static-mandelbrot-color-presets-js*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/js/mandelbrot"
												:name "color-presets"
												:type "js"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-color-presets-js ()
	(load-js-content *static-mandelbrot-color-presets-js*))

(push (create-regex-dispatcher "^/js/mandelbrot/color-presets.js$" 'mandelbrot-color-presets-js) *dispatch-table*)



(defvar *static-mandelbrot-initialize-js*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/js/mandelbrot"
												:name "initialize"
												:type "js"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-initialize-js ()
	(load-js-content *static-mandelbrot-initialize-js*))

(push (create-regex-dispatcher "^/js/mandelbrot/initialize.js$" 'mandelbrot-initialize-js) *dispatch-table*)



(defvar *static-mandelbrot-debug-js*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/js/mandelbrot"
												:name "debug"
												:type "js"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-debug-js ()
	(load-js-content *static-mandelbrot-debug-js*))

(push (create-regex-dispatcher "^/js/mandelbrot/debug.js$" 'mandelbrot-debug-js) *dispatch-table*)



(defvar *static-mandelbrot-benchmark-js*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/js/mandelbrot"
												:name "benchmark"
												:type "js"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-benchmark-js ()
	(load-js-content *static-mandelbrot-benchmark-js*))

(push (create-regex-dispatcher "^/js/mandelbrot/benchmark.js$" 'mandelbrot-benchmark-js) *dispatch-table*)



(defvar *static-mandelbrot-resize-canvas-js*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/js/mandelbrot"
												:name "resize-canvas"
												:type "js"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-resize-canvas-js ()
	(load-js-content *static-mandelbrot-resize-canvas-js*))

(push (create-regex-dispatcher "^/js/mandelbrot/resize-canvas.js$" 'mandelbrot-resize-canvas-js) *dispatch-table*)



(defvar *static-mandelbrot-paint-canvas-js*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/js/mandelbrot"
												:name "paint-canvas"
												:type "js"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-paint-canvas-js ()
	(load-js-content *static-mandelbrot-paint-canvas-js*))

(push (create-regex-dispatcher "^/js/mandelbrot/paint-canvas.js$" 'mandelbrot-paint-canvas-js) *dispatch-table*)



(defvar *static-mandelbrot-user-events-js*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/js/mandelbrot"
												:name "user-events"
												:type "js"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-user-events-js ()
	(load-js-content *static-mandelbrot-user-events-js*))

(push (create-regex-dispatcher "^/js/mandelbrot/user-events.js$" 'mandelbrot-user-events-js) *dispatch-table*)



(defvar *static-mandelbrot-settings-js*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/js/mandelbrot"
												:name "settings"
												:type "js"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-settings-js ()
	(load-js-content *static-mandelbrot-settings-js*))

(push (create-regex-dispatcher "^/js/mandelbrot/settings.js$" 'mandelbrot-settings-js) *dispatch-table*)



(defvar *static-mandelbrot-utilities-js*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/js/mandelbrot"
												:name "utilities"
												:type "js"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-utilities-js ()
	(load-js-content *static-mandelbrot-utilities-js*))

(push (create-regex-dispatcher "^/js/mandelbrot/utilities.js$" 'mandelbrot-utilities-js) *dispatch-table*)



(defvar *static-left-click-svg*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/images/"
												:name "left-click"
												:type "svg"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-left-click-svg ()
	(load-svg-content *static-left-click-svg*))

(push (create-regex-dispatcher "^/images/left-click.svg$" 'mandelbrot-left-click-svg) *dispatch-table*)



(defvar *static-zoom-in-svg*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/images/"
												:name "zoom-in"
												:type "svg"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-zoom-in-svg ()
	(load-svg-content *static-zoom-in-svg*))

(push (create-regex-dispatcher "^/images/zoom-in.svg$" 'mandelbrot-zoom-in-svg) *dispatch-table*)



(defvar *static-right-click-svg*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/images/"
												:name "right-click"
												:type "svg"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-right-click-svg ()
	(load-svg-content *static-right-click-svg*))

(push (create-regex-dispatcher "^/images/right-click.svg$" 'mandelbrot-right-click-svg) *dispatch-table*)



(defvar *static-zoom-out-svg*
	(load-time-value
	 (with-open-file (in (make-pathname
												:directory "/home/path/firefractal/static/images/"
												:name "zoom-out"
												:type "svg"
												:version nil
												:defaults *this-file*)
											 :element-type 'flex:octet)
		 (let ((html-data (make-array (file-length in)
																	:element-type 'flex:octet)))
			 (read-sequence html-data in)
			 html-data))))

(defun mandelbrot-zoom-out-svg ()
	(load-svg-content *static-zoom-out-svg*))

(push (create-regex-dispatcher "^/images/zoom-out.svg$" 'mandelbrot-zoom-out-svg) *dispatch-table*)

