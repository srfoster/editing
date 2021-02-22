#lang at-exp racket

(provide get-from-url
	 after-download
	; get-from-yt-url
	 )

(require "./fetch-util.rkt")

(define clip-id 0)
(define (next-clip-name ext)
  (set! clip-id (add1 clip-id))
  (~a "clip" clip-id ext))

(define after-download (make-parameter
			 (lambda (in-file out-file)
			   (system
			     @~a{ffmpeg -i @in-file -vcodec libx264 -vf "[in]scale=iw*min(600/iw\,400/ih):ih*min(600/iw\,400/ih)[scaled]; [scaled]pad=600:400:(600-iw*min(600/iw\,400/ih))/2:(400-ih*min(600/iw\,400/ih))/2[padded];[padded]setsar=1:1[out]" -r 60 -strict experimental -y @out-file}
			     ))))

(define (get-from-url 
	  #:wget-params [wget-params '()]
	  #:file-extension [file-extension ".mp4"]
	  #:out-file-extension [out-file-extension ".mp4"]
	  #:working-directory [working-directory
				(build-path (current-directory) "input")]
	  url [out-file #f])
  (if (get-from-index url)
      (build-path working-directory (get-from-index url))
      (let ()
	(when (not out-file)
	  (set! out-file
	    (next-clip-name out-file-extension)))
	(define fn (make-temporary-file (~a "rkttmp~a" file-extension)
					#f 
					working-directory))

	(wget wget-params url fn)

	((after-download) fn (build-path working-directory out-file))

	(store-in-index url out-file)
	(build-path working-directory out-file)
	)
      ))


(define (wget wget-params url fn)
  (system
    @~a{wget @(string-join wget-params " ") @url -O @fn}))



#;
(define (get-from-yt-url url out-file)
  (define fn (make-temporary-file "rkttmp~a" 
				  #f 
				  (build-path (current-directory) "input")))

  (system
    @~a{youtube-dl -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/mp4' --output @|fn|.mp4 @url})
  (system
    @~a{ffmpeg -i @|fn|.mp4 -vcodec libx264 -vf scale=600:400,setsar=1:1 -r 60 -strict experimental -y input/@out-file}))
