#lang at-exp racket

(provide get-from-url
	; get-from-yt-url
	 )

(require "./fetch-util.rkt")

(define (get-from-url 
	  #:wget-params [wget-params '()]
	  #:file-extension [file-extension '()]
	  #:working-directory [working-directory
				(build-path (current-directory) "input")]
	  url out-file)
  (if (get-from-index url)
      (get-from-index url)
      (let ()
	(define fn (make-temporary-file (~a "rkttmp~a" file-extension)
					#f 
					working-directory))

	(wget wget-params url fn)

	(system
	  @~a{ffmpeg -i @fn -vcodec libx264 -vf "[in]scale=iw*min(600/iw\,400/ih):ih*min(600/iw\,400/ih)[scaled]; [scaled]pad=600:400:(600-iw*min(600/iw\,400/ih))/2:(400-ih*min(600/iw\,400/ih))/2[padded]; [padded]setsar=1:1[out]" -r 60 -strict experimental -y input/@out-file})
	(store-in-index url out-file)
	out-file)))


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
