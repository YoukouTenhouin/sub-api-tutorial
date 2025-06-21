#lang racket

(provide
 bili-fetch-sub-records
 (struct-out bili-gift-record))

(require net/http-easy
         (only-in net/cookies list-cookie-jar%))
(require gregor)

(require "qr-code.rkt")

(define user-agent
  (string-append
   "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) "
   "Chrome/137.0.0.0 Safari/537.36"))

(define (unwrap-bili-res r)
  (let* ([json (response-json r)]
         [code (hash-ref json 'code)])
    (if (equal? code 0)
        (hash-ref json 'data)
        (error "Bilibili API code ~a: ~a" code (hash-ref json 'message)))))

(define (bili-login-get-code)
  (let ([res (get "https://passport.bilibili.com/x/passport-login/web/qrcode/generate")])
    (unwrap-bili-res res)))

(define (bili-login-poll-result key)
  (let ([res (get "https://passport.bilibili.com/x/passport-login/web/qrcode/poll"
                  #:params `([qrcode_key . ,key]))])
    (let ([code (hash-ref (unwrap-bili-res res) 'code)])
      (case code
        [(0) #t]
        [(86101) 'not-scanned]
        [(86090) 'scanned]
        [(86038) 'expired]
        [else (error "Unknown code when polling QR status: ~a" code)]))))

(define (bili-login-poll-loop key [scanned-notified #f])
  (case (bili-login-poll-result key)
    [(#t) (display "登录成功。\n") #t]
    [(expired) (error "QR code expired")]
    [(not-scanned) (bili-login-poll-loop key scanned-notified)]
    [(scanned)
     (unless scanned-notified
       (display "请在手机上确认登录。\n"))
     (bili-login-poll-loop key #t)]))

(define (bili-login)
  (let ([res (bili-login-get-code)])
    (let ([url (hash-ref res 'url)]
          [key (hash-ref res 'qrcode_key)])
      (qr-code url)
      (bili-login-poll-loop key))))

(define (bili-user-info)
  (let* ([res (get "https://api.bilibili.com/x/web-interface/nav")]
         [json (response-json res)])
    (let ([code (hash-ref json 'code)]
          [data (hash-ref json 'data)])
      (case code
        [(0) (printf "已登录用户：~a UID:~a\n"
                     (hash-ref data 'uname)
                     (hash-ref data 'mid))]
        [(-101) (display "未登录\n")]
        [else (error "Bilibili API code ~a: ~a" code (hash-ref json 'message))]))))

(define (bili-fetch-records-page #:date date
                                 #:gift_id [gift_id #f]
                                 #:uname [uname #f]
                                 #:last_id [last_id #f]
                                 #:coin_type [coin_type #f])
  (let ([res (get (string-append "https://api.live.bilibili.com/xlive/revenue/v1"
                                 "/giftStream/getReceivedGiftStreamNextList")
                  #:params `([begin_time . ,date]
                             [gift_id . ,gift_id]
                             [coin_type . ,coin_type]
                             [uname . ,uname]
                             [last_id . ,last_id]
                             [limit . "20"]))])
    (unwrap-bili-res res)))

(define (print-fetch-info gift_id date page)
  (when gift_id (printf "礼物编号~a " gift_id))
  (when date (printf "日期~a " date))
  (when page (printf "第~a页\n" page)))

(define (bili-fetch-records-for-date
         date
         #:gift_id [gift_id #f]
         #:uname [uname #f]
         #:coin_type [coin_type #f])
  (let fetch-page ([records empty]
                   [last_id #f]
                   [has-more #t]
                   [page 1])
    (if has-more
        (begin
          (print-fetch-info gift_id date page)
          (let ([res (bili-fetch-records-page
                      #:date date
                      #:gift_id gift_id
                      #:uname uname
                      #:last_id last_id
                      #:coin_type coin_type)])
            (let* ([list (hash-ref res 'list)]
                   [has-more (not (= 0 (hash-ref res 'has_more)))]
                   [last_id (and has-more (number->string (hash-ref (last list) 'id)))])
              (fetch-page (cons list records) last_id has-more (+ page 1)))))
        (append* records))))

(define (bili-fetch-records-date-range begin-date [end-date (today)]
                                       #:gift_id [gift_id #f]
                                       #:uname [uname #f]
                                       #:coin_type [coin_type #f])
  (let fetch ([current begin-date]
              [records empty])
    (if (date<=? current end-date)
        (let ([date-str (~t current "yyyy-MM-dd")])
          (let ([res (bili-fetch-records-for-date
                      date-str
                      #:gift_id gift_id
                      #:uname uname
                      #:coin_type coin_type)])
            (fetch (+days current 1) (cons res records))))
        (append* records))))

(struct bili-gift-record
  (id
   name
   count
   sender-name
   sender-uid
   timestamp
   silver
   total-hamster
   ios-hamster
   normal-hamster
   total-gold
   ios-gold
   normal-gold)
  #:transparent)

(define (time-string->timestamp str)
  (->posix (parse-moment str "yyyy-MM-dd HH:mm:ss")))

(define (api-json->gift-record json)
  (let ([json (hash-set json 'time (time-string->timestamp (hash-ref json 'time)))])
    (apply bili-gift-record (map (curry hash-ref json)
                            '(gift_id
                              gift_name
                              gift_num
                              uname
                              uid
                              time
                              silver
                              hamster
                              ios_hamster
                              normal_hamster
                              gold
                              ios_gold
                              normal_gold)))))

(define (reconcile-records records)
  (define ret (vector-map! api-json->gift-record (list->vector records)))
  (vector-sort! ret (lambda (x y) (< (bili-gift-record-id x) (bili-gift-record-id y))))
  ret)

(define (bili-fetch-gift-records begin-date [end-date (today)]
                                 #:gift_id [gift_id #t]
                                 #:uname [uname #f]
                                 #:coin_type [coin_type #f])
  (bili-login)
  (bili-user-info)
  (reconcile-records
   (if (list? gift_id)
       (append* empty
                (map
                 (lambda (g)
                   (bili-fetch-records-date-range begin-date end-date
                                                  #:gift_id g
                                                  #:uname uname
                                                  #:coin_type coin_type))
                 gift_id))
       (bili-fetch-records-date-range begin-date end-date
                                      #:gift_id gift_id
                                      #:uname uname
                                      #:coin_type coin_type))))

(define (run-in-context fun . args)
  (let ([jar (new list-cookie-jar%)])
    (parameterize ([current-user-agent user-agent]
                   [current-session (make-session #:cookie-jar jar)]
                   [current-timezone "Asia/Shanghai"])
      (apply fun args))))

(define (bili-fetch-sub-records begin-date [end-date (today #:tz "Asia/Shanghai")])
  (run-in-context (lambda () (bili-fetch-gift-records
                              begin-date end-date #:gift_id '("10001" "10002" "10003")))))
