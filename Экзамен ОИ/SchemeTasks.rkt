;1--------------------------------------------------------------------

(define (isUpperAlphabetic? x)
  (and (char? x) (>= (char->integer x) 65) (<= (char->integer x) 90)))

;3--------------------------------------------------------------------

(define (remove-repeats xs)
  (let loop ((x xs) (known '()) (res '()))
    (if (null? x)
        (reverse res)
        (if (member (car x) known)
            (loop (cdr x) known res)
            (loop (cdr x) (cons (car x) known) (cons (car x) res))))))

;5--------------------------------------------------------------------

(define (my-filter pr? xs)
  (if (null? xs)
      '()
      (if (pr? (car xs))
          (cons (car xs) (my-filter pr? (cdr xs)))
          (my-filter pr? (cdr xs)))))

;7--------------------------------------------------------------------

(define (my-map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (my-map f (cdr xs)))))

           
;10--------------------------------------------------------------------

(define (my-list-ref xs target)
  (and (>= (- (length xs) 1) target)
       (let loop ((x xs) (n 0))
         (if (not (= n target))
             (loop (cdr x) (+ n 1))
             (car x)))))

;1 реализация
(define (my-length1 xs)
  (let loop ((x xs) (c 0))
    (if (null? x)
        c
        (loop (cdr x) (+ 1 c)))))
      
;2 реализация
(define (my-length2 xs)
  (if (null? xs)
      0
      (+ 1 (my-length2 (cdr xs)))))

(define (my-reverse xs)
  (let loop ((x xs) (res '()))
    (if (null? x)
        res
        (loop (cdr x) (cons (car x) res)))))

;12--------------------------------------------------------------------

(define (counterPr pr? xs)
  (if (null? xs)
      0
      (if (pr? (car xs))
          (+ 1 (counterPr pr? (cdr xs)))
          (counterPr pr? (cdr xs)))))

;13--------------------------------------------------------------------

(define (conv-sum xs)
  (if (null? xs)
      0
      (+ (car xs) (conv-sum (cdr xs)))))

(define (conv-mul xs)
  (if (null? xs)
      1
      (* (car xs) (conv-mul (cdr xs)))))

(define (conv-min xs)
  (if (null? xs)
      1e9
      (min (car xs) (conv-min (cdr xs)))))

(define (conv-max xs)
  (if (null? xs)
      -1e9
      (max (car xs) (conv-max (cdr xs)))))


;15--------------------------------------------------------------------

(define (recast xs)
  (and (equal? (car xs) '+) (= (length xs) 3) (list? (list-ref xs 1)) (list? (list-ref xs 2))
       (= (length (list-ref xs 1)) 3) (= (length (list-ref xs 2)) 3)
       (equal? (cadr (list-ref xs 1)) (cadr (list-ref xs 2)))
       (list '* (cadr (list-ref xs 1))(list (car xs) (caddr (list-ref xs 1)) (caddr (list-ref xs 2))))))
      
;17--------------------------------------------------------------------

(define (symbols-append . xs)
  (define (helper xs)
    (if (null? xs)
        ""
        (string-append (symbol->string (car xs)) (helper (cdr xs)))))
  (string->symbol (helper xs)))

;19--------------------------------------------------------------------

(define count
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      n)))

;24--------------------------------------------------------------------

(define (drop xs target)
  (let loop ((x xs) (n 0))
    (and (>= (length xs) target)
         (if (= n target)
             x
             (loop (cdr x) (+ 1 n))))))