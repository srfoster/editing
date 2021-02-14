#lang at-exp racket

(provide atrim volume)

(provide trim 
	 scale
	 overlay
	 concat
	 vflip
	 hflip
	 setpts
	 vignette
	 hue
	 xfade ;need ffmpeg 4.3
	 blend
	 )


(require editing/base)

;AUDIO


;For the user's convenience, we autoconvert sources to audio sources when they are used in an audio filter
(define (->a s)
  (if (source? s)
      (filt (list s)
	    #:audio (list s)
	    ;Same source, volume of 1.  Noop, but now is a filter returning an audio source
	    @~a{volume=volume=1})
      s))

(define (atrim i #:start [start 0] #:duration [duration 1])
  (filt (list (->a i))
	@~a{atrim=start=@|start|:duration=@|duration|}))


(define (volume i #:volume [volume 1])
  (filt (list (->a i))
	@~a{volume=volume=@|volume|}))


;VIDEO

(define (trim i #:start [start 0] #:duration [duration 1])
  (filt (list i)
	@~a{trim=start=@|start|:duration=@|duration|}))

(define (scale i #:w [w 600] #:h [h 480])
  (filt (list i)
	@~a{scale=iw*min(@|w|/iw\,@|h|/ih):ih*min(@|w|/iw\,@|h|/ih),pad=@|w|:@|h|:(@|w|-iw*min(@|w|/iw\,@|h|/ih))/2:(@|h|-ih*min(@|w|/iw\,@|h|/ih))/2,setsar=1:1}))


(define (concat-2 a b)
  (filt (list a b) "concat"))

(define (concat . ins)
  (if (= 2 (length ins))
      (concat-2 (first ins) (second ins))
      (concat-2 (first ins) (apply concat (rest ins)))))

(define (blank 
	  dur)
  (filt (list)
	(~a "nullsrc=size=600x400,trim=start=0:duration=" dur)))

(define (vflip i)
  (filt (list i)
	"vflip"))

(define (hflip i)
  (filt (list i)
	"hflip"))

(define (overlay i1 i2
		 #:x [x 0]
		 #:y [y 0])
  (filt (list i1 i2)
	@~a{overlay=x=@|x|:y=@|y|}))

(define (setpts #:expression expr i)
  (filt (list i)
	@~a{setpts=@|expr|}))

(define (vignette i)
  (filt (list i)
	@~a{vignette=PI/4}))

(define (hue i)
  (filt (list i)
	@~a{hue=90}))


(define (xfade x y)
  (filt (list x y)
	@~a{xfade=transition=fade:duration=0.5}))

(define (blend x y #:all-expr [all-expr ""])
  (filt (list x y)
	@~a{blend=all_expr='@|all-expr|'}))
