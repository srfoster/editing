#lang at-exp racket

(require editing)
	 
(working-directory "workspace")

;TODO:
;text
;fade transition
;blending
;animation / easings

;audio fx
;audio mixing/looping 

(define w 1920)
(define h 1080)

(define plane
  (file-source 
    #:args "-loop 1"
    @~a{"plane.png"}))

(define trucks
  (file-source 
    #:args "-loop 1"
    @~a{"trucks.png"}))


(begin
  (render #:to "output.mp4"
	  (concat 
	    (scale 
	      #:w w #:h h 
	      (trim plane  #:start 0 #:duration 1))
	    (scale 
	      #:w w #:h h 
	      (trim trucks  #:start 0 #:duration 1))
	    (scale 
	      #:w w #:h h 
	      (trim plane  #:start 0 #:duration 1))

	    ;Doesn't render.   Why???
	    #;
	    (scale 
	      #:w w #:h h 
	      (trim trucks  #:start 0 #:duration 1))
	    ))

  (system "xdg-open workspace/output.mp4"))

#;
(begin
  (render #:to "output.mp4"
	  (blend 
	    #:all-expr 
	    ;animation
	    "if(gte(Y-N*SH*10,0),A,B)"
	    ;"if(eq(mod(X,2),mod(Y,2)),A,B)"
	    ;"A*(X/W)+B*(1-X/W)"
	    (scale 
	      #:w w #:h h 
	      (trim plane  #:start 0 #:duration 10))
	    (scale 
	      #:w w #:h h 
	      (trim trucks  #:start 0 #:duration 10))
	    ))

  (system "xdg-open workspace/output.mp4"))

#;
(begin
  (render #:to "output.mp4"
	  (vignette
	    (hue
	      (overlay
		#:x (/ w 4) #:y (/ h 4)
		(scale 
		  #:w w #:h h 
		  (trim plane  #:start 0 #:duration 1))
		(vignette
		  (setpts
		    #:expression "PTS+1/TB" ;Input plus 1 second
		    (scale 
		      #:w (/ w 2) #:h (/ h 2)
		      (trim trucks #:start 0 #:duration 1))))))))

  (system "xdg-open workspace/output.mp4"))

;Concat two images as videos

#;
(begin 
  (define plane
    (file-source 
      #:args "-loop 1"
      @~a{"plane.png"}))

  (define trucks
    (file-source 
      #:args "-loop 1"
      @~a{"trucks.png"}))

  (render #:to "output.mp4"
	  (concat
	    (scale 
	      #:w 1920 #:h 1080 
	      (trim plane  #:start 0 #:duration 1))
	    (scale 
	      #:w 1920 #:h 1080 
	      (trim trucks #:start 0 #:duration 1))))

  (system "xdg-open workspace/output.mp4"))

#;
(begin 
  (define audio
    (file-source "voiceover.mp4"))

  (define video
    (file-source 
      #:args "-loop 1"
      @~a{"plane.png"}))

  (render #:to "output.mp4"
	  (trim 
	    #:duration 2
	    (filt (list video) "vflip"))

	  (atrim
	    #:duration 2
	    (volume
	      #:volume 0.5
	      audio)))

  (system "xdg-open workspace/output.mp4"))



