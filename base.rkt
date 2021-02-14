#lang at-exp racket 

(provide
  (struct-out source)
  (except-out (struct-out filt) filt)
  (rename-out [make-filt filt]))

(struct source  (args thunk))

(struct filt (ins audio-ins string) #:transparent)

(define (make-filt ins string
		   #:audio [audio-ins '()])
  (filt ins 
	audio-ins
	string))

