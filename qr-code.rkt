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

(define (qr-code-fallback text)
  (let ([temp-file (get-temp-file)])
    (qr-write text temp-file)
    (case (system-type 'os)
      [(windows)
       (let ([start (find-executable-path "start")])
         (system* start temp-file))]
      ([unix]
       (let ([xdg-open (find-executable-path "xdg-open")])
         (system* xdg-open temp-file))))))

(define (qr-code text)
  (let ([qrencode (find-executable-path qr-command)])
    (if qrencode
        (system* qrencode "-t" "UTF8" text)
        (qr-code-fallback text))))
