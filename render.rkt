#lang at-exp racket 

(provide render
	 new-render
	 url-source file-source image-source
	 working-directory
	 filt-compile
	 current-sources
	 show
	 debug?
	 current-width
	 current-height
	 )

(require editing/fetch-util)
(require editing/base)

(define debug? (make-parameter #f))
(define working-directory (make-parameter (current-directory)))
(define current-sources (make-parameter '()))
(define current-mappings (make-parameter #f))

(define current-width (make-parameter #f))
(define current-height (make-parameter #f))

(define (->path s)
  (if (absolute-path? s)
      s
      (build-path (working-directory) s)))

(define file-id 0)
(define (next-file-name ext)
  (set! file-id (add1 file-id))
  (~a "file" file-id "." ext))

(define (url->file-type s)
  (local-require net/url)
  (define u (string->url s))
  (define path (path/param-path (last (url-path u))))

  (substring path
	     (- (string-length path) 3)))

(define (url-source 
	  #:args [args ""]
	  #:file-ext [file-ext #f]
	  #:wget-params [wget-params ""]
	  url)
  (source args
    (thunk
      (define fn (next-file-name
		      (url->file-type url)))

      (define out (->path fn))

      (index (->path "index"))

      (if (get-from-index url)
	  (->path (get-from-index url))
	  (let ()
	    (system
	      @~a{wget @wget-params @url -O @out})
	    (store-in-index url fn)
	    out))
      )))

(define (file-source #:args [args ""]
		     path)
  (source
    args
    (thunk (->path path))))

(define (image-source #:args [args ""]
		      image)
  (local-require (only-in 2htdp/image save-image))
  (source
    (~a args " -loop 1")
    (thunk 
      (define f (make-temporary-file "~a.png"))
      (save-image image f)
      f)))

(define (fresh-filt-var)
  (~a "[a"(random 1000)"]"))


(define audio-sources (make-parameter '()))
(define (get-filt-var s)
  (if (source? s)
      (~a "[" (index-of (current-sources) s) 
	  ":" 
	  (if (member s (audio-sources)) "a" "v") 
	  "]")
      (~a "[" (last (string-split s "[")))))


(define (filt-compile f)
  (define (->in x)
    (if (source? x)
	(let () x)
	(let ()
	  (define fc (filt-compile x))
	  fc)))

  (define pres 
    (map ->in (filt-ins f)))

  (define ins 
    (parameterize ([audio-sources (filt-audio-ins f)])
      (map get-filt-var pres)))

  (define filt-var
    (fresh-filt-var)
    )

  (define parts
    (append (filter (not/c source?) pres) 
	    (list
	      (~a
		(string-join ins "")
		(filt-string f)
		filt-var))))

  (when (current-mappings)
    (hash-set! (current-mappings)
	       f
	       filt-var)) 

  (~a (string-join 
	parts
	";")))


(define (render 
	  #:show? [show? #t]
	  #:to to video-filt [audio-filt #f])
  (define sources (filt->sources video-filt))
  (define audio-sources 
    (if (not audio-filt)
        '()
	(filt->sources audio-filt)))

  (define input-args (map
		       (lambda (c)
			 (~a (source-args c) 
			     " -i " 
			     ((source-thunk c))))
		       (append sources
			       audio-sources)))
    
  (define filter-complex 
    (parameterize ([current-sources (append sources audio-sources)])
      (filt-compile video-filt)))

  (define filter-complex-audio
    (parameterize ([current-sources (append sources audio-sources)])
      (if (not audio-filt)
	  #f
	  (filt-compile audio-filt))))

  ((if (debug?) displayln system)
    @~a{ffmpeg @(string-join input-args " ") -filter_complex "@|filter-complex|@(if (not audio-filt) "" @~a{;@filter-complex-audio})" -map @(get-filt-var filter-complex) @(if (not audio-filt) "" @~a{-map @(get-filt-var filter-complex-audio)})  @(->path to)})
  
  (when show?
    (show to)))

(define (new-render 
	  the-filt
	  #:show? [show? #t]
	  #:to to 
	  #:maps [output-maps (list the-filt)])
  (define sources (filt->sources the-filt))

  (define input-args (map
		       (lambda (c)
			 (~a (source-args c) 
			     " -i " 
			     ((source-thunk c))))
		       sources))
    
  (define mappings (make-hash))

  (define filter-complex
    (parameterize ([current-sources sources]
		   [current-mappings mappings])
      (filt-compile the-filt)))

  (define map-args
    (string-join
      (map 
	(lambda (m) 
	  (~a "-map " (hash-ref mappings m)))
	output-maps)
      " "))

  ((if (debug?) displayln system)
    @~a{ffmpeg @(string-join input-args " ") -filter_complex "@|filter-complex|"  @|map-args| @(->path to)})
  
  (when show?
    (show to)))


(define (filt->sources f)
  (if (source? f)
      (list f)
      (flatten
	(append
	  (map filt->sources (filt-ins f))))))


(define (show f)
  (when (not (debug?))
    (system 
      @~a{xdg-open @(->path f)})))


