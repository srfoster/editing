#lang at-exp racket

;For working with files generated by
; Amazon's transcription service

(provide json-file->transcribed-words
	 keep-words
	 (struct-out transcribed-word))

(require json)

(struct transcribed-word (start-time end-time string))

(define (json-file->transcribed-words json-file)
  (define s (file->string "e1-vo.json"))

  (define j (string->jsexpr s))

  (define items (hash-ref (hash-ref j 'results) 'items))

  (define times+words
    ;Only return ones with valid time stamp
    (filter transcribed-word-start-time
	    (map 
	      (lambda (i)
		(define et (hash-ref i 'end_time #f))
		(define st (hash-ref i 'start_time #f))
		(transcribed-word 
		  (and st (string->number st))
		  (and et (string->number et))
		  (string-downcase (hash-ref (first  (hash-ref i 'alternatives)) 'content))))
	      items)))

  times+words)

(define (keep-words words . to-keep)
  (if (empty? to-keep)
      '()
      (let ()
	(define current   (first to-keep))   

	(define current-i (index-of (map transcribed-word-string words) current string=?))

	(append
	  (list (list-ref words current-i)) 
	  (apply keep-words (drop words  current-i) 
		 (rest to-keep))))))




