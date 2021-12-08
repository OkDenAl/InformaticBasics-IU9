(define fast-break 1)

(define (token->number token)
  (string->number (list->string token)))


(define (tokenizer-frac expr)
  (let loop ((lstr expr) (res '()) (i 0))
    (if (not (null? lstr))
        (if (equal? (car lstr) #\/)
            (let ((tail (tokenizer-tail (cdr lstr))))
              (if (not (null? tail))
                  (list (cons 'numerator  (reverse res)) (cons 'denomirator (reverse tail)))
                  '()))
            (if (or (and (member (car lstr) '(#\+ #\-)) (= i 0)) (char-numeric? (car lstr)))
                (loop (cdr lstr) (cons (car lstr) res) (+ i 1))
                (loop '() '() 0)))
        '())))

(define (tokenizer-tail expr)
  (let loop ((lstr expr) (res '()))
    (if (null? lstr)
        res
        (if (char-numeric? (car lstr))
            (loop (cdr lstr) (cons (car lstr) res))
            (loop '() '())))))
            
        
(define (check-frac str)
  (not(null? (tokenizer-frac (string->list str)))))

(define (scan-frac str)
  (and (check-frac str)
       (let ((tokens (tokenizer-frac (string->list str))))
         (eval (list '/ (token->number (cdr (assoc 'numerator tokens)))
                     (token->number (cdr(assoc 'denomirator tokens))))(interaction-environment)))))

(define (scan-many-fracs str)
  (define (helper expr)
    (let loop ((lstr expr) (curres '()) (res '()) (counter 0))
      (if (null? lstr)
          (if (> counter 0)
              (let ((scanner (scan-frac (list->string curres))))
                  (and scanner
                       (append res (list (scan-frac (list->string curres))))))
              res)
          (if (char-whitespace? (car lstr))
              (if (> counter 0)
                  (let ((scanner (scan-frac (list->string curres))))
                  (and scanner
                       (loop (cdr lstr) '() (append res (list scanner)) 0)))
                  (loop (cdr lstr) '() res 0))
              (loop (cdr lstr) (append curres (list (car lstr))) res (+ counter 1))))))
  (helper (string->list str)))
              


(check-frac "110/111") 
(check-frac "-4/3")    
(check-frac "+5/10")   
(check-frac "5.0/10")  
(check-frac "FF/10")

(scan-frac "110/111")  
(scan-frac "-4/3")     
(scan-frac "+5/10")    
(scan-frac "5.0/10")   
(scan-frac "FF/10")

(scan-many-fracs
 "\t1/2 1/3\n\n10/8") 
(scan-many-fracs
 "\t1/2 1/3\n\n2/-5")