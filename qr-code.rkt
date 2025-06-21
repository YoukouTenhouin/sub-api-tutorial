#lang racket

(provide qr-code)

(require racket/system)
(require simple-qr)

(define qr-command "qrencode")

(define (get-temp-file)
  (let ([temp-dir (find-system-path 'temp-dir)])
    (build-path
     temp-dir
     (string-append "bili-qr-login-" (number->string (current-milliseconds)) ".png"))))

(define (platform-file-opener)
  (find-executable-path
   (case (system-type 'os)
     [(windows) "exploere"]
     [(unix) "xdg-open"]
     ; This is provided to me by Gemini. I don't own a Mac so I can't test it.
     ; If this ever blows up... Well it's your fault for using a Mac.
     [(macos) "open"])))

(define (qr-code-fallback text)
  (let ([temp-file (get-temp-file)])
    (qr-write text temp-file)
    (system* (platform-file-opener) temp-file)))

(define (qr-code text)
  (let ([qrencode (find-executable-path qr-command)])
    (if qrencode
        (system* qrencode "-t" "UTF8" text)
        (qr-code-fallback text))))
