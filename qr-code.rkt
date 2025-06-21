#lang racket

(provide qr-code)

(require racket/system)

(define qr-command "qrencode")

(define (write-qr-text text)
  (printf "系统上找不到`~a`命令。\n" qr-command)
  (display "请手动将下列内容转换为二维码：\n")
  (printf "~a\n" text))

(define (qr-code text)
  (let ([qrencode (find-executable-path qr-command)])
    (if qrencode
        (system* qrencode "-t" "UTF8" text)
        (write-qr-text text))))
