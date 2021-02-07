#lang racket

(provide fetch-scrape)

(define (fetch-scrape url regexp)
  (remove-duplicates
    (regexp-match*
      regexp
      (with-output-to-string
	(thunk
	  (system
	    (~a "curl \""url"\"")))))))

