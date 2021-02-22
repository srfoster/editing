#lang racket

(provide store-in-index get-from-index index clear-index)

(define index (make-parameter #f))

(define (store-in-index key val)
  (with-output-to-file (index)
		       #:exists 'append
		       (thunk
			 (displayln (~a key "--->" val)))))

(define (get-from-index key)
  (and (not (index))
       (raise "Must call get-from-index after setting index param"))

  (if (not (file-exists? (index)))
      #f
      (let ()
	(define lines (file->lines (index)))

	(define h (apply hash (flatten (map (curryr string-split "--->") lines))))

	(hash-ref h (~a key) #f))))


(define (clear-index)
    (and (file-exists? (index))
	 (delete-file (index))))

(module+ test
	 (require rackunit) 

	 (index "test-index")
	 (clear-index)

	 (store-in-index "A" "1")

	 (check-equal?
	   (get-from-index "A")
	   "1")

	 (check-equal?
	   (get-from-index "B")
	   #f))


