#lang at-exp racket 

(provide render
	 concat overlay
	 url-source file-source
	 working-directory
	 filt-compile
	 blank
	 (struct-out source)
	 (except-out (struct-out filt) filt)
	 (rename-out [make-filt filt])
	 current-sources)

(require editing/fetch-util)

(struct source  (args thunk))

(define working-directory (make-parameter (current-directory)))
(define current-sources (make-parameter '()))

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
	  #:file-ext [file-ext #f]
	  #:wget-params [wget-params ""]
	  url)
  (source ""
    (thunk
      (define fn (next-file-name
		      (url->file-type url)))

      (define out (build-path (working-directory) fn))

      (index (build-path (working-directory) "index"))

      (if (get-from-index url)
	  (build-path (working-directory) (get-from-index url))
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
    (thunk (build-path (working-directory) path))))

(struct filt (ins audio-ins string) #:transparent)

(define (make-filt ins string
		   #:audio [audio-ins '()])
  (displayln "FILT")
  (filt ins 
	audio-ins
	string))

(define (concat-2 a b)
  (make-filt (list a b) "concat"))

(define (concat . ins)
  (if (= 2 (length ins))
      (concat-2 (first ins) (second ins))
      (concat-2 (first ins) (apply concat (rest ins)))))

(define (overlay i1 i2)
  (make-filt (list i1 i2)
	     "overlay"))

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
	x
	(let ()
	  (define fc (filt-compile x))
	  fc)))

  (define pres 
    (map ->in (filt-ins f)))

  (define ins 
    (parameterize ([audio-sources (filt-audio-ins f)])
      (map get-filt-var pres)))

  (define parts
    (append (filter (not/c source?) pres) 
	    (list
	      (~a
		(string-join ins "")
		(filt-string f)
		(fresh-filt-var)))))

  (~a (string-join 
	parts
	";")))


(define (render #:to to video-filt [audio-filt #f])
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

  (system
    @~a{ffmpeg @(string-join input-args " ") -filter_complex "@|filter-complex|@(if (not audio-filt) "" @~a{;@filter-complex-audio})" -map @(get-filt-var filter-complex) @(if (not audio-filt) "" @~a{-map @(get-filt-var filter-complex-audio)}) -shortest @(build-path (working-directory) to)}))


(define (filt->sources f)
  (if (source? f)
      (list f)
      (flatten
	(append
	  (map filt->sources (filt-ins f))))))




(define (blank dur)
  (filt (list)
	(~a "nullsrc=size=600x400,trim=start=0:duration=" dur)))


