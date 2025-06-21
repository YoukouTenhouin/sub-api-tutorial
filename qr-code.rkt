#lang typed/racket

(provide qr-code)

(require typed/racket/system)
(require/typed simple-qr
               [qr-write (-> String Path-String Void)])

(define qr-command "qrencode")

(define (get-temp-file)
  (let ([temp-dir (find-system-path 'temp-dir)])
    (build-path
     temp-dir
     (string-append "bili-qr-login-" (number->string (current-milliseconds)) ".png"))))

(define (qr-code-fail-message text)
  (displayln "无法生生生生成二维码。")
  (displayln "请手动将如下文本转换为二维码，并用Bilibili客户端扫描：")
  (displayln text))

(define (platform-file-opener)
  (find-executable-path
   (case (system-type 'os)
     [(windows) "exploere"]
     [(unix) "xdg-open"]
     ; This is provided to me by Gemini. I don't own a Mac so I can't test it.
     ; If this ever blows up... Well it's your fault for using a Mac.
     [(macos) "open"]
     [else (error "Unknown platform")])))

(define (qr-code-fallback [text : String])
  (let ([temp-file (get-temp-file)]
        [file-opener (platform-file-opener)])
    (if [and temp-file file-opener]
        (begin
          (qr-write text temp-file)
          (system* file-opener temp-file))
        (qr-code-fail-message text))))

(define (qr-code [text : String])
  (let ([qrencode (find-executable-path qr-command)])
    (if qrencode
        (system* qrencode "-t" "UTF8" text)
        (qr-code-fallback text))))
