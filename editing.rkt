#lang at-exp racket

(provide concat 
	 show
	 combine
	 combine2)

(provide overlay-audio)

(struct filter (ins content out))

(define (compile-filter f)
  (define (brackets s) (~a "[" s "]"))

  (define ins (map brackets (filter-ins f)))
  (define out (brackets (filter-out f)))

  (~a (string-join ins "") (filter-content f) out))

(define (compile-filters . fs)
  (string-join (map compile-filter (flatten fs))
	       ";"))

(define (concat-filter . ins+out)
  (filter (drop-right ins+out 1) 
	  "concat"
	  (last ins+out)))



(define (combine c1 c2 out)
  (system
    @~a{
    ffmpeg -i input/@c1 -i input/@c2 -filter_complex "[0:v]trim=start=0:duration=3[a];[0:v]trim=start=1:duration=3,setpts=PTS-STARTPTS[b];[b]hflip[c];[a][c]concat[d];[1:v]trim=start=0:duration=3,setpts=PTS-STARTPTS[e];[d][e]concat[out1]" -map [out1] input/@out
    }))

(define (combine2 #:length [l 1] . cs)
  (define letters '(a b c d e f g h i))
  (define is (map (lambda (c)
		    (~a "-i input/" c))
		  (drop-right cs 1)
		  ))
  (define clips (map 
		    (lambda (c i)
		      (~a "[" i ":v]trim=start=0:duration=" l (if (> i 0) ",setpts=PTS-STARTPTS" "") "[clip" i "]"))
		    (drop-right cs 1)
		    (range 0 (length is))))


  (define (concats ins)
    (define r (random 10000))
    (if (= 2 (length ins))
	(list; @~a{[@(first ins)][@(second ins)]concat[out1]}
	       (concat-filter (first ins) (second ins) 'out1))
	(cons
	  ;@~a{[@(first ins)][@(second ins)]concat[temp@r]}
	  (concat-filter (first ins) (second ins) (~a "temp" r))
	  (concats (cons (~a "temp" r) 
			 (drop ins 2))))))

  (system
    @~a{
    ffmpeg @(string-join is " ") -filter_complex "@(string-join clips ";");@(compile-filters (concats (map (lambda (i) (~a "clip" i)) (range 0 (length is)))))" -map [out1] input/@(last cs)
    }))

(define (concat l out-file)
  (with-output-to-file 
    #:exists 'replace
    (build-path (current-directory) "concat_list.txt")
    (thunk*
      (map (lambda(p)
	     (displayln (~a "file input/" p)))
	   l)))

  (system (~a "ffmpeg -f concat -safe 0 -i concat_list.txt -c copy -y input/" out-file)))

(define (show f)
  (system (~a "xdg-open input/" f)))




(define (overlay-audio i1 i2 out)
  (system
    @~a{
    ffmpeg -i input/@i1 -i input/@i2 -map 1:v -map 0:a -c:v copy -shortest input/@out
    }))
