

(define (memoized-factorial n)
  (let ((known-results '()))
    (define (loop n)
      (cond
        ((<= n 1) 1)
        (else(let((res (assoc n known-results)))
               (if res
                   (cadr res)
                   (begin
                     (set! res (* (loop(- n 1)) n))
                     (set! known-results (cons (cons n res) known-results))
                     res))))))
    (loop n)))

;2------------------------------------------------------------------

(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b)
     (cons a (delay b)))))

(define (lazy-car p)
  (car p))

(define (lazy-cdr p)
  (force (cdr p)))

(define (lazy-head xs k)
   (if (= k 0)
       '()
       (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))))

(define (lazy-ref xs k)
  (define (loop xs n)
    (if (= n k)
        (lazy-car xs)
        (loop (lazy-cdr xs) (+ n 1))))
  (loop xs 0))

(define (naturals start)
  (lazy-cons start (naturals (+ start 1))))

(define (fact)
  (define (loop n p)
      (lazy-cons  (* n p) (loop (+ n 1) (* n p))))
  (loop 1 1))

(define (lazy-factorial n)
  (lazy-ref (fact) n))

;3------------------------------------------------------------------

(define (read-words)
    (define (loop c word ans)
      (if (eof-object? (peek-char))
          (begin
            (if (> c 0)
                (reverse (cons word ans))
                (reverse ans)))
          (let ((char (read-char)))
            (if (char-whitespace? char)
                (if (> c 0)
                    (loop 0 "" (cons word ans))
                    (loop 0 "" ans))
                (loop (+ c 1) (string-append word (string char)) ans)))))
    (loop 0 "" '()))

;4------------------------------------------------------------------

(define ie (interaction-environment))

(define-syntax define-struct
  (syntax-rules()
    ((define-struct name (f_n1 ...))
     (begin
       (eval (list 'define (string->symbol (string-append "make-" (symbol->string 'name)))
                   (lambda (f_n1 ...)
                     `(,(list 'struct 'name) ,(list 'f_n1 f_n1) ...)))ie)
       (eval (list 'define (string->symbol (string-append (symbol->string 'name) "?"))
                   (lambda (var)
                     (and (list? var) (not (null? var)) (list? (car var)) (equal? (caar var) 'struct) (equal? (cadar var) 'name)))) ie)
       (eval (list 'define (string->symbol (string-append (symbol->string 'name) "-" (symbol->string 'f_n1)))
                   (lambda (var)
                     (let ((res (assoc 'f_n1 (cdr var))))
                       (and res (cadr res))))) ie) ...
       (eval (list 'define (string->symbol (string-append "set-" (symbol->string 'name) "-" (symbol->string 'f_n1) "!"))
                   (lambda (var value)
                       (and (list? var) (not (null? var))(set-car! (cdr (assoc 'f_n1 (cdr var))) value)))) ie) ...))))





