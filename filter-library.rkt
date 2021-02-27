#lang at-exp racket

(provide atrim 
	 volume
	 adelay
	 asetpts)

(provide trim 
	 scale
	 rotate
	 overlay
	 concat
	 new-concat
	 vflip
	 hflip
	 setpts
	 vignette
	 hue
	 xfade ;need ffmpeg 4.3
	 blend
	 blank
	 text
	 setsar
	 loop
	 fps

	 geq
	 hue
	 
	 ;Combos. Probably move to different file?
	 scale/pad

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

(define (adelay i #:delays [d #f]
		#:all [a #f])
  (filt (list (->a i))
	@~a{adelay=@(attr 'delays d)@(attr 'all a)}))

(define (atrim i #:start [start 0] #:duration [duration 1])
  (filt (list (->a i))
	@~a{atrim=start=@|start|:duration=@|duration|}))


(define (volume i #:volume [volume 1])
  (filt (list (->a i))
	@~a{volume=volume=@|volume|}))

(define (asetpts #:expression expr i)
  (filt (list i)
	@~a{asetpts=@|expr|}))

;VIDEO

(define (trim i #:start [start 0] #:duration [duration 1])
  (filt (list i)
	@~a{trim=start=@|start|:duration=@|duration|}))

(define (scale i #:w [w #f] #:h [h #f])
  (filt (list i)
	@~a{scale=@(attr 'w w)@(attr 'h h)}

	))

(define (rotate i #:rotate [r 0])
  (filt (list i)
	@~a{rotate=@|r|:c=none}))

(define (scale/pad i #:w [w #f] #:h [h #f]
		   #:color [c "0x00000000"])
  (filt (list i)
	@~a{scale=iw*min(@|w|/iw\,@|h|/ih):ih*min(@|w|/iw\,@|h|/ih),format=rgba,pad=@|w|:@|h|:(@|w|-iw*min(@|w|/iw\,@|h|/ih))/2:(@|h|-ih*min(@|w|/iw\,@|h|/ih))/2:color=@|c|,setsar=1:1}
  ))

(define (setsar i #:sar [sar "1:1"])
  (filt (list i)
	@~a{setsar=@(attr 'sar sar)}
	)) 


(define (concat-2 a b)
  (filt (list a b) "concat"))

(define (concat . ins)
  (if (= 2 (length ins))
      (concat-2 (first ins) (second ins))
      (concat-2 (first ins) (apply concat (rest ins)))))

(define (new-concat #:n [n 2] #:v [v 1] #:a [a 0] . ins)
  (filt ins
	@~a{concat=@(attr 'n n)@(attr 'v v)@(attr 'a a)})) 

(define (vflip i)
  (filt (list i)
	"vflip"))

(define (hflip i)
  (filt (list i)
	"hflip"))

(define (overlay i1 i2
		 #:x [x 0]
		 #:y [y 0])
  (filt (list i2 i1)
	@~a{overlay=x=@|x|:y=@|y|}))

(define (setpts #:expression expr i)
  (filt (list i)
	@~a{setpts=@|expr|}))

(define (vignette i)
  (filt (list i)
	@~a{vignette=PI/4}))


(define (xfade x y)
  (filt (list x y)
	@~a{xfade=transition=fade:duration=0.5}))

(define (blend x y #:all-expr [all-expr ""])
  (filt (list x y)
	@~a{blend=all_expr='@|all-expr|'}))


(define (blank
	  #:w [w 640]
	  #:h [h 480]
	  #:color [c "0x00000000"]
	  )
  (filt (list)
	@~a{nullsrc=s=1x1,format=rgba,pad=width=@|w|:height=@|h|:color=@|c|}))

(define (geq i)
  ;Just a demo for now...
  (filt (list i)
    @~a{geq=random(1)/hypot(X-cos(N*0.07)*W/2-W/2\,Y-sin(N*0.09)*H/2-H/2)^2*1000000*sin(N*0.02):128:128}))


(define (attr n v)
  (if v (~a n "=" v ":") ""))

(define (text i
	      #:text t
	      #:enable [e #f]
	      #:fontsize [f #f]
	      #:fontfile [ff #f]
	      #:fontcolor [fc "white"]
	      #:x [x "(w-text_w)/2"]
	      #:y [y "(h-text_h)/2"])
  ;Consider using textfile=@|some-temp-file|
  (define temp (make-temporary-file))
  (with-output-to-file
    #:exists 'replace 
    temp
    (thunk
      (display
	(string-replace t
			"%" "\\%"))))
  (filt 
    (list i)
    @~a{drawtext=@(attr 'enable e)@(attr 'x x)@(attr 'y y)@(attr 'fontsize f)@(attr 'fontfile ff)@(attr 'fontcolor fc)textfile=@temp}))


(define (loop i #:loop l
	      #:size [s #f]
	      #:start [st #f])
  (filt 
    (list i)
    @~a{loop=@(attr 'loop l)@(attr 'size s)@(attr 'start st)}))

(define (fps i #:fps f)
  (filt 
    (list i)
    @~a{fps=@(attr 'fps f)}))

(define (hue i #:h [h #f])
  (filt 
    (list i)
    @~a{hue=@(attr 'h h)}))

